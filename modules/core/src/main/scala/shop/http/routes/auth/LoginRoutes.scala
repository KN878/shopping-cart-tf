package shop.http.routes.auth

import cats.Defer
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import shop.algebras.Auth
import shop.domain.auth.{ InvalidUsernameOrPassword, LoginUser }
import shop.effects._
import shop.http.decoder._
import shop.http.json._

final class LoginRoutes[F[_]: Defer: MonadThrow: JsonDecoder](
    auth: Auth[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/auth"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "login" =>
      req.decodeR[LoginUser] { user =>
        auth
          .login(user.username.toDomain, user.password.toDomain)
          .flatMap(Ok(_))
          .recoverWith {
            case InvalidUsernameOrPassword(_) =>
              Forbidden()
          }
      }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
