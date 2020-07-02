package shop.algebras

import cats.effect.{ Resource, Sync }
import cats.implicits._
import shop.http.auth.users._
import shop.domain.auth._
import shop.effects._
import shop.extensions.skunkx._
import skunk._
import skunk.codec.all._
import skunk.implicits._

trait Users[F[_]] {
  def find(
      userName: UserName,
      password: Password
  ): F[Option[User]]

  def create(
      userName: UserName,
      password: Password
  ): F[UserId]
}

final class LiveUsers[F[_]: BracketThrow: GenUUID] private (
    sessionPool: Resource[F, Session[F]],
    crypto: Crypto
) extends Users[F] {
  import UserQueries._

  override def find(userName: UserName, password: Password): F[Option[User]] =
    sessionPool.use { s =>
      s.prepare(selectUser)
        .use { ps =>
          ps.option(userName)
        }
        .map {
          case Some(u ~ p) =>
            if (p.value == crypto.encrypt(password).value)
              u.some
            else
              none[User]
          case _ => none[User]
        }
    }

  override def create(userName: UserName, password: Password): F[UserId] =
    sessionPool.use { s =>
      s.prepare(insertUser).use { cmd =>
        GenUUID[F].make[UserId].flatMap { id =>
          val ePass = crypto.encrypt(password)
          cmd
            .execute(User(id, userName) ~ ePass)
            .as(id)
            .handleErrorWith {
              case SqlState.UniqueViolation(_) =>
                UsernameInUse(userName).raiseError[F, UserId]
            }
        }
      }
    }
}

object LiveUsers {
  def make[F[_]: Sync](pool: Resource[F, Session[F]], crypto: Crypto): F[LiveUsers[F]] =
    Sync[F].delay {
      new LiveUsers[F](pool, crypto)
    }
}

private object UserQueries {
  val codec: Codec[User ~ EncryptedPassword] =
    (uuid.cimap[UserId] ~ varchar.cimap[UserName] ~ varchar.cimap[EncryptedPassword]).imap {
      case i ~ n ~ p => User(i, n) ~ p
    } {
      case u ~ p =>
        u.id ~ u.name ~ p
    }

  val selectUser: Query[UserName, User ~ EncryptedPassword] =
    sql"""
         SELECT *
         FROM users
         WHERE name = ${varchar.cimap[UserName]}
         """.query(codec)

  val insertUser: Command[User ~ EncryptedPassword] =
    sql"""
         INSERT INTO users
         VALUES ($codec)
         """.command
}
