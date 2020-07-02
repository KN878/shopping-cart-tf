package shop.http.routes.secured

import cats.Defer
import cats.implicits._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.{ AuthMiddleware, Router }
import org.http4s.{ AuthedRoutes, HttpRoutes }
import shop.domain.checkout.Card
import shop.domain.order.{ EmptyCartError, OrderError, PaymentError }
import shop.domain.shoppingCart.CartNotFound
import shop.effects._
import shop.http.auth.users.CommonUser
import shop.http.decoder._
import shop.services.CheckoutService
import shop.http.json._

final class CheckoutRoutes[F[_]: Defer: MonadThrow: JsonDecoder](
    checkoutService: CheckoutService[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/checkout"

  private val httpRoutes: AuthedRoutes[CommonUser, F] = AuthedRoutes.of {
    case req @ POST -> Root as user =>
      req.req.decodeR[Card] { card =>
        checkoutService
          .checkout(user.value.id, card)
          .flatMap(Created(_))
          .recoverWith {
            case CartNotFound(userId) =>
              NotFound(
                s"Cart not found for user: $userId"
              )
            case EmptyCartError =>
              BadRequest("Shopping cart is empty")
            case PaymentError(cause) =>
              BadRequest(cause)
            case OrderError(cause) =>
              BadRequest(cause)
          }
      }
  }

  def routes(authMiddleware: AuthMiddleware[F, CommonUser]): HttpRoutes[F] = Router(
    prefixPath -> authMiddleware(httpRoutes)
  )
}
