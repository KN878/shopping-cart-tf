package shop.http.routes.auth

import cats.Defer
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import shop.algebras.Auth
import shop.domain.auth.{ CreateUser, UsernameInUse }
import shop.effects._
import shop.http.decoder._
import shop.http.json._

final class RegistrationRoutes[F[_]: Defer: MonadThrow: JsonDecoder](
    auth: Auth[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/auth"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "users" =>
      req.decodeR[CreateUser] { user =>
        auth
          .newUser(
            user.username.toDomain,
            user.password.toDomain
          )
          .flatMap(Created(_))
          .recoverWith {
            case UsernameInUse(username) =>
              Conflict(s"Username ${username.value} is already taken")
          }
      }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
