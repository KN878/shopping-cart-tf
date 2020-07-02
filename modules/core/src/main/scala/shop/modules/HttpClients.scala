package shop.modules

import cats.effect.Sync
import org.http4s.client.Client
import shop.config.data.PaymentConfig
import shop.http.clients.{ LivePaymentClient, PaymentClient }

trait HttpClients[F[_]] {
  def payment: PaymentClient[F]
}

object HttpClients {
  def make[F[_]: Sync](
      paymentConfig: PaymentConfig,
      client: Client[F]
  ): F[HttpClients[F]] =
    Sync[F].delay {
      new HttpClients[F] {
        override def payment: PaymentClient[F] = new LivePaymentClient[F](paymentConfig, client)
      }
    }
}
