package shop.http.routes.admin

import cats.Defer
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.{ AuthMiddleware, Router }
import org.http4s.{ AuthedRoutes, HttpRoutes }
import shop.algebras.Categories
import shop.domain.category.CategoryParam
import shop.effects._
import shop.http.auth.users.AdminUser
import shop.http.decoder._
import shop.http.json._

final class AdminCategoryRoutes[F[_]: Defer: MonadThrow: JsonDecoder](
    categories: Categories[F]
) extends Http4sDsl[F] {
  private[admin] val prefixPath = "/category"

  private val httpRoutes: AuthedRoutes[AdminUser, F] = AuthedRoutes.of {
    case req @ POST -> Root as _ =>
      req.req.decodeR[CategoryParam] { c =>
        Created(categories.create(c.toDomain))
      }
  }

  def routes(
      authMiddleware: AuthMiddleware[F, AdminUser]
  ): HttpRoutes[F] = Router(
    prefixPath -> authMiddleware(httpRoutes)
  )
}
