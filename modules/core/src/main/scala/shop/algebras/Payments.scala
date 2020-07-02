package shop.algebras

import shop.domain.order.PaymentId
import shop.domain.payment.Payment

trait Payments[F[_]] {
  def process(payment: Payment): F[PaymentId]
}
