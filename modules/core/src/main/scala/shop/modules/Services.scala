package shop.modules

import cats.effect.{ Sync, Timer }
import cats.implicits._
import eu.timepit.refined.auto._
import io.chrisdavenport.log4cats.Logger
import retry.RetryPolicies._
import retry.RetryPolicy
import shop.config.data.CheckoutConfig
import shop.effects.{ Background, MonadThrow }
import shop.services.CheckoutService

final class Services[F[_]: Background: Logger: MonadThrow: Timer] private (
    checkoutConfig: CheckoutConfig,
    algebras: Algebras[F],
    clients: HttpClients[F]
) {
  val retryPolicy: RetryPolicy[F] =
    limitRetries[F](checkoutConfig.retriesLimit) |+|
        exponentialBackoff(checkoutConfig.retriesBackoff)

  def checkout: CheckoutService[F] = new CheckoutService[F](
    clients.payment,
    algebras.cart,
    algebras.orders,
    retryPolicy
  )
}

object Services {
  def make[F[_]: Background: Logger: Sync: Timer](
      checkoutConfig: CheckoutConfig,
      algebras: Algebras[F],
      clients: HttpClients[F]
  ): F[Services[F]] = Sync[F].delay {
    new Services[F](checkoutConfig, algebras, clients)
  }
}
