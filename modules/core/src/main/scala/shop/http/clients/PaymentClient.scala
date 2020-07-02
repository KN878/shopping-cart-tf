package shop.http.clients

import cats.implicits._
import eu.timepit.refined.auto._
import org.http4s.Method._
import org.http4s._
import org.http4s.circe._
import org.http4s.client._
import org.http4s.client.dsl.Http4sClientDsl
import shop.config.data.PaymentConfig
import shop.domain.order.{ PaymentError, PaymentId }
import shop.domain.payment.Payment
import shop.effects._
import shop.http.json._

trait PaymentClient[F[_]] {
  def process(payment: Payment): F[PaymentId]
}

class LivePaymentClient[F[_]: MonadThrow: JsonDecoder](
    cfg: PaymentConfig,
    client: Client[F]
) extends PaymentClient[F]
    with Http4sClientDsl[F] {

  override def process(payment: Payment): F[PaymentId] =
    Uri
      .fromString(cfg.uri.value + "/payments")
      .liftTo[F]
      .flatMap { uri =>
        client.fetch[PaymentId](POST(payment, uri)) { r =>
          if (r.status == Status.Ok || r.status == Status.Conflict)
            r.asJsonDecode[PaymentId]
          else
            PaymentError(
              Option(r.status.reason).getOrElse("Unknown")
            ).raiseError[F, PaymentId]
        }
      }
}
