package shop.modules

import cats._
import cats.effect._
import cats.implicits._
import dev.profunktor.redis4cats.RedisCommands
import shop.algebras._
import shop.config.data.ShoppingCartExpiration
import skunk.Session

final class Algebras[F[_]] private (
    val cart: ShoppingCart[F],
    val brands: Brands[F],
    val categories: Categories[F],
    val items: Items[F],
    val orders: Orders[F],
    val healthCheck: HealthCheck[F]
)

object Algebras {
  def make[F[_]: Concurrent: Parallel: Timer](
      redis: RedisCommands[F, String, String],
      sessionPool: Resource[F, Session[F]],
      cartExpiration: ShoppingCartExpiration
  ): F[Algebras[F]] =
    for {
      brands <- LiveBrands.make[F](sessionPool)
      categories <- LiveCategories.make[F](sessionPool)
      orders <- LiveOrders.make[F](sessionPool)
      items <- LiveItems.make[F](sessionPool)
      cart <- LiveShoppingCart.make[F](items, redis, cartExpiration)
      healthCheck <- LiveHealthCheck.make[F](redis, sessionPool)
    } yield new Algebras[F](cart, brands, categories, items, orders, healthCheck)
}
