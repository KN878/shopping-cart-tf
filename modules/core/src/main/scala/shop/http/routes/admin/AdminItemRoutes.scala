package shop.http.routes.admin

import cats.Defer
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.{ AuthMiddleware, Router }
import org.http4s.{ AuthedRoutes, HttpRoutes }
import shop.algebras.Items
import shop.domain.item.{ CreateItemParam, UpdateItemParam }
import shop.effects._
import shop.http.auth.users.AdminUser
import shop.http.decoder._
import shop.http.json._

final class AdminItemRoutes[F[_]: Defer: MonadThrow: JsonDecoder](
    items: Items[F]
) extends Http4sDsl[F] {
  private[admin] val prefixPath = "/items"

  private val httpRoutes: AuthedRoutes[AdminUser, F] = AuthedRoutes.of {
    // Create new item
    case req @ POST -> Root as _ =>
      req.req.decodeR[CreateItemParam] { item =>
        Created(items.create(item.toDomain))
      }

    // Update item
    case req @ PUT -> Root as _ =>
      req.req.decodeR[UpdateItemParam] { item =>
        Ok(items.update(item.toDomain))
      }
  }

  def routes(
      authMiddleware: AuthMiddleware[F, AdminUser]
  ): HttpRoutes[F] = Router(
    prefixPath -> authMiddleware(httpRoutes)
  )
}
