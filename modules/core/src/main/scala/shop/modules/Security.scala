package shop.modules

import cats.effect.{ Resource, Sync }
import cats.implicits._
import dev.profunktor.auth.jwt._
import dev.profunktor.redis4cats.RedisCommands
import eu.timepit.refined.auto._
import io.circe.parser.{ decode => jsonDecode }
import pdi.jwt._
import shop.algebras._
import shop.config.data._
import shop.domain.auth._
import shop.effects._
import shop.http.auth.users._
import skunk.Session

final class Security[F[_]] private (
    val auth: Auth[F],
    val adminAuth: UsersAuth[F, AdminUser],
    val userAuth: UsersAuth[F, CommonUser],
    val adminJwtAuth: AdminJwtAuth,
    val userJwtAuth: UserJwtAuth
)

object Security {
  def make[F[_]: Sync](
      cfg: AppConfig,
      sessionPool: Resource[F, Session[F]],
      redis: RedisCommands[F, String, String]
  ): F[Security[F]] = {
    val adminJwtAuth = AdminJwtAuth(
      JwtAuth.hmac(cfg.adminJwtConfig.secretKeyConfig.value.value, JwtAlgorithm.HS256)
    )

    val userJwtAuth = UserJwtAuth(
      JwtAuth.hmac(cfg.tokenConfig.value.value, JwtAlgorithm.HS256)
    )

    val adminToken = JwtToken(
      cfg.adminJwtConfig.adminToken.value.value
    )

    for {
      adminClaim <- jwtDecode[F](adminToken, adminJwtAuth.value)
      content <- ApThrow[F].fromEither(jsonDecode[ClaimContent](adminClaim.content))
      adminUser = AdminUser(User(UserId(content.uuid), UserName("admin")))
      tokens <- LiveTokens.make[F](cfg.tokenConfig, cfg.tokenExpiration)
      crypto <- LiveCrypto.make[F](cfg.passwordSalt)
      users <- LiveUsers.make[F](sessionPool, crypto)
      auth <- LiveAuth.make[F](cfg.tokenExpiration, tokens, users, redis)
      adminAuth <- LiveAdminAuth.make[F](adminToken, adminUser)
      userAuth <- LiveUsersAuth.make[F](redis)
    } yield new Security(auth, adminAuth, userAuth, adminJwtAuth, userJwtAuth)
  }
}
