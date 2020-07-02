package shop.algebras

import cats._
import cats.effect.Sync
import cats.implicits._
import dev.profunktor.auth.jwt.JwtToken
import dev.profunktor.redis4cats.RedisCommands
import io.circe.parser.decode
import io.circe.syntax._
import pdi.jwt.JwtClaim
import shop.config.data.TokenExpiration
import shop.domain.auth._
import shop.effects._
import shop.http.auth.users._
import shop.http.json._

trait Auth[F[_]] {
  def newUser(username: UserName, password: Password): F[JwtToken]
  def login(username: UserName, password: Password): F[JwtToken]
  def logout(token: JwtToken, username: UserName): F[Unit]
}

final class LiveAuth[F[_]: GenUUID: MonadThrow] private (
    tokenExpiration: TokenExpiration,
    tokens: Tokens[F],
    users: Users[F],
    redis: RedisCommands[F, String, String]
) extends Auth[F] {
  override def newUser(username: UserName, password: Password): F[JwtToken] =
    users.find(username, password).flatMap {
      case Some(_) => UsernameInUse(username).raiseError[F, JwtToken]
      case None =>
        for {
          uId <- users.create(username, password)
          token <- tokens.create
          user = User(uId, username).asJson.noSpaces
          _ <- redis.setEx(token.value, user, tokenExpiration.value)
          _ <- redis.setEx(username.value, token.value, tokenExpiration.value)
        } yield token
    }

  override def login(username: UserName, password: Password): F[JwtToken] =
    users.find(username, password).flatMap {
      case None => InvalidUsernameOrPassword(username).raiseError[F, JwtToken]
      case Some(user) =>
        redis.get(username.value).flatMap {
          case Some(token) => JwtToken(token).pure[F]
          case None =>
            tokens.create.flatTap { t =>
              redis.setEx(t.value, user.asJson.noSpaces, tokenExpiration.value) *>
                redis.setEx(username.value, t.value, tokenExpiration.value)
            }
        }
    }

  override def logout(token: JwtToken, username: UserName): F[Unit] =
    redis.del(token.value) *> redis.del(username.value)
}

object LiveAuth {
  def make[F[_]: Sync](
      tokenExpiration: TokenExpiration,
      tokens: Tokens[F],
      users: Users[F],
      redis: RedisCommands[F, String, String]
  ): F[LiveAuth[F]] =
    Sync[F].delay {
      new LiveAuth[F](tokenExpiration, tokens, users, redis)
    }
}

trait UsersAuth[F[_], A] {
  def findUser(token: JwtToken)(claim: JwtClaim): F[Option[A]]
}

final class LiveUsersAuth[F[_]: Functor] private (
    redis: RedisCommands[F, String, String]
) extends UsersAuth[F, CommonUser] {
  override def findUser(token: JwtToken)(claim: JwtClaim): F[Option[CommonUser]] =
    redis
      .get(token.value)
      .map(_.flatMap { u =>
        decode[User](u).toOption.map(CommonUser.apply)
      })
}

object LiveUsersAuth {
  def make[F[_]: Sync](redis: RedisCommands[F, String, String]): F[LiveUsersAuth[F]] =
    Sync[F].delay {
      new LiveUsersAuth[F](redis)
    }
}

final class LiveAdminAuth[F[_]: Applicative] private (
    adminToken: JwtToken,
    adminUser: AdminUser
) extends UsersAuth[F, AdminUser] {
  override def findUser(token: JwtToken)(claim: JwtClaim): F[Option[AdminUser]] =
    (token == adminToken)
      .guard[Option]
      .as(adminUser)
      .pure[F]
}

object LiveAdminAuth {
  def make[F[_]: Sync](adminToken: JwtToken, adminUser: AdminUser): F[LiveAdminAuth[F]] =
    Sync[F].delay {
      new LiveAdminAuth[F](adminToken, adminUser)
    }
}
