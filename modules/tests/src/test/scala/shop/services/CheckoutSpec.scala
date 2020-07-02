package shop.services

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits.{ catsSyntaxEq => _, _ }
import retry.RetryPolicies._
import retry.RetryPolicy
import shop.algebras.{ Orders, ShoppingCart }
import shop.arbitraries._
import shop.domain.auth._
import shop.domain.checkout._
import shop.domain.item.ItemId
import shop.domain.order._
import shop.domain.payment._
import shop.domain.shoppingCart._
import shop.http.clients.PaymentClient
import squants.market._
import suite._

final class CheckoutSpec extends PureTestSuite {
  val MaxRetries                     = 3
  val retryPolicies: RetryPolicy[IO] = limitRetries[IO](MaxRetries)

  def successfulPaymentClient(pid: PaymentId): PaymentClient[IO] =
    new PaymentClient[IO] {
      override def process(payment: Payment): IO[PaymentId] = IO.pure(pid)
    }

  def successfulCart(cartTotal: CartTotal): ShoppingCart[IO] =
    new ShoppingCart[IO] {
      override def add(userId: UserId, itemId: ItemId, quantity: Quantity): IO[Unit] = ???

      override def delete(userId: UserId): IO[Unit] = IO.unit

      override def get(userId: UserId): IO[CartTotal] = IO.pure(cartTotal)

      override def removeItem(userId: UserId, itemId: ItemId): IO[Unit] = ???

      override def update(userId: UserId, cart: Cart): IO[Unit] = ???
    }

  def successfulOrders(oid: OrderId): Orders[IO] =
    new Orders[IO] {
      override def get(userId: UserId, orderId: OrderId): IO[Option[Order]] = ???

      override def findBy(userId: UserId): IO[List[Order]] = ???

      override def create(
          userId: UserId,
          pid: PaymentId,
          items: List[CartItem],
          total: Money
      ): IO[OrderId] =
        IO.pure(oid)
    }

  val emptyCart: ShoppingCart[IO] =
    new ShoppingCart[IO] {
      override def add(userId: UserId, itemId: ItemId, quantity: Quantity): IO[Unit] = ???

      override def delete(userId: UserId): IO[Unit] = ???

      override def get(userId: UserId): IO[CartTotal] =
        IO.pure(CartTotal(List.empty, USD(0)))

      override def removeItem(userId: UserId, itemId: ItemId): IO[Unit] = ???

      override def update(userId: UserId, cart: Cart): IO[Unit] = ???
    }

  val unreachablePaymentClient: PaymentClient[IO] =
    new PaymentClient[IO] {
      override def process(payment: Payment): IO[PaymentId] = IO.raiseError(PaymentError("error"))
    }

  def recoveringPaymentClient(
      retries: Ref[IO, Int],
      pid: PaymentId
  ): PaymentClient[IO] =
    new PaymentClient[IO] {
      override def process(payment: Payment): IO[PaymentId] =
        retries.get.flatMap {
          case n if n == 1 =>
            IO.pure(pid)
          case _ =>
            retries.update(_ + 1) *> IO.raiseError(PaymentError("error"))
        }
    }

  val failingOrders: Orders[IO] =
    new Orders[IO] {
      override def get(userId: UserId, orderId: OrderId): IO[Option[Order]] = ???

      override def findBy(userId: UserId): IO[List[Order]] = ???

      override def create(userId: UserId, pid: PaymentId, items: List[CartItem], total: Money): IO[OrderId] =
        IO.raiseError(OrderError("error"))
    }

  def failingCart(ct: CartTotal): ShoppingCart[IO] =
    new ShoppingCart[IO] {
      override def add(userId: UserId, itemId: ItemId, quantity: Quantity): IO[Unit] = ???

      override def delete(userId: UserId): IO[Unit] =
        IO.raiseError(new Exception("error"))

      override def get(userId: UserId): IO[CartTotal] = IO.pure(ct)

      override def removeItem(userId: UserId, itemId: ItemId): IO[Unit] = ???

      override def update(userId: UserId, cart: Cart): IO[Unit] = ???
    }

  test("failed to delete cart does not affect order") {
    implicit val bg = shop.background.NoOp
    import shop.logger.NoOp
    forAll { (uid: UserId, oid: OrderId, card: Card, pid: PaymentId, ct: CartTotal) =>
      IOAssertion {
        new CheckoutService[IO](successfulPaymentClient(pid), failingCart(ct), successfulOrders(oid), retryPolicies)
          .checkout(uid, card)
          .map(id => assert(id === oid))
      }
    }
  }

  test("cannot create order, run in background") {
    forAll { (uid: UserId, ct: CartTotal, card: Card, pid: PaymentId) =>
      IOAssertion {
        Ref.of[IO, Int](0).flatMap { counter =>
          Ref.of[IO, List[String]](List.empty).flatMap { logs =>
            implicit val bg     = shop.background.counter(counter)
            implicit val logger = shop.logger.acc(logs)
            new CheckoutService[IO](
              successfulPaymentClient(pid),
              successfulCart(ct),
              failingOrders,
              retryPolicies
            ).checkout(uid, card)
              .attempt
              .flatMap {
                case Left(OrderError(_)) =>
                  (counter.get, logs.get).mapN {
                    case (c, (x :: y :: xs)) =>
                      assert(
                        x.contains("Rescheduling") &&
                          y.contains("Giving up") &&
                          xs.size == MaxRetries &&
                          c == 1
                      )
                    case _ =>
                      fail(s"Expected $MaxRetries retries and reschedule")
                  }
                case _ =>
                  fail("Expected order error")
              }
          }
        }
      }
    }
  }

  test("recovering after 1 try payment client") {
    forAll { (uid: UserId, ct: CartTotal, oid: OrderId, card: Card, pid: PaymentId) =>
      IOAssertion {
        Ref.of[IO, List[String]](List.empty).flatMap { logs =>
          Ref.of[IO, Int](0).flatMap { retries =>
            implicit val bg     = shop.background.NoOp
            implicit val logger = shop.logger.acc(logs)
            new CheckoutService[IO](
              recoveringPaymentClient(retries, pid),
              successfulCart(ct),
              successfulOrders(oid),
              retryPolicies
            ).checkout(uid, card)
              .attempt
              .flatMap {
                case Right(id) =>
                  logs.get.map { xs =>
                    assert(id == oid && xs.length == 1)
                  }
                case _ =>
                  fail("Expected order id")
              }
          }
        }
      }
    }
  }

  test("unreachable payment client") {
    forAll { (uid: UserId, ct: CartTotal, oid: OrderId, card: Card) =>
      IOAssertion {
        Ref.of[IO, List[String]](List.empty).flatMap { logs =>
          implicit val bg     = shop.background.NoOp
          implicit val logger = shop.logger.acc(logs)
          new CheckoutService[IO](unreachablePaymentClient, successfulCart(ct), successfulOrders(oid), retryPolicies)
            .checkout(uid, card)
            .attempt
            .flatMap {
              case Left(PaymentError(_)) =>
                logs.get.map {
                  case x :: xs =>
                    assert(x.contains("Giving up") && xs.size == MaxRetries)
                  case _ =>
                    fail(s"Expected $MaxRetries retries")
                }
              case _ =>
                fail("Expected payment error")
            }
        }
      }
    }
  }

  test("empty cart checkout") {
    implicit val bg = shop.background.NoOp
    import shop.logger.NoOp
    forAll { (uid: UserId, pid: PaymentId, oid: OrderId, card: Card) =>
      IOAssertion {
        new CheckoutService[IO](successfulPaymentClient(pid), emptyCart, successfulOrders(oid), retryPolicies)
          .checkout(uid, card)
          .attempt
          .map {
            case Left(EmptyCartError) =>
              assert(true)
            case _ =>
              fail("Cart was not empty as expected")
          }
      }
    }
  }

  test("successful checkout") {
    implicit val bg = shop.background.NoOp
    import shop.logger.NoOp
    forAll { (uid: UserId, pid: PaymentId, oid: OrderId, ct: CartTotal, card: Card) =>
      IOAssertion {
        new CheckoutService[IO](successfulPaymentClient(pid), successfulCart(ct), successfulOrders(oid), retryPolicies)
          .checkout(uid, card)
          .map(id => assert(oid === id))
      }
    }
  }
}
