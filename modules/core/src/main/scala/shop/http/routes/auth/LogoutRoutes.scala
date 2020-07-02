package shop.http.routes.auth

import cats.Defer
import cats.implicits._
import dev.profunktor.auth.AuthHeaders
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.{ AuthMiddleware, Router }
import org.http4s.{ AuthedRoutes, HttpRoutes }
import shop.algebras.Auth
import shop.effects._
import shop.http.auth.users.CommonUser

final class LogoutRoutes[F[_]: Defer: MonadThrow: JsonDecoder](
    auth: Auth[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/auth"

  private val httpRoutes: AuthedRoutes[CommonUser, F] = AuthedRoutes.of {
    case req @ POST -> Root / "logout" as user =>
      AuthHeaders
        .getBearerToken(req.req)
        .traverse_(token => auth.logout(token, user.value.name)) *> NoContent()
  }

  def routes(
      authMiddleware: AuthMiddleware[F, CommonUser]
  ): HttpRoutes[F] = Router(
    prefixPath -> authMiddleware(httpRoutes)
  )
}
