package shop.http.routes.secured

import java.util.UUID

import cats.data.Kleisli
import cats.effect.IO
import org.http4s.server.AuthMiddleware
import org.http4s._
import org.http4s.Method._
import org.http4s.client.dsl.io._
import shop.algebras.ShoppingCart
import shop.arbitraries._
import shop.domain.auth.{ UserId, UserName }
import shop.domain.item.ItemId
import shop.domain.{ auth, item }
import shop.domain.shoppingCart._
import shop.http.auth.users._
import shop.http.json._
import squants.market.USD
import suite.{ HttpTestSuite, IOAssertion }

final class CartRoutesSpec extends HttpTestSuite {
  val authUser = CommonUser(
    User(UserId(UUID.randomUUID()), UserName("user"))
  )

  val authMiddleWare: AuthMiddleware[IO, CommonUser] =
    AuthMiddleware(Kleisli.pure(authUser))

  def dataCart(ct: CartTotal) = new TestCart {
    override def get(userId: UserId): IO[CartTotal] =
      IO.pure(ct)
  }

  def failingCart(ct: CartTotal) = new TestCart {
    override def get(userId: UserId): IO[CartTotal] =
      IO.raiseError(DummyError) *> IO.pure(ct)
  }

  test("GET shopping cart") {
    forAll { ct: CartTotal =>
      IOAssertion {
        GET(Uri.unsafeFromString("/cart")).flatMap { req =>
          val routes = new CartRoutes[IO](dataCart(ct)).routes(authMiddleWare)
          assertHttp(routes, req)(Status.Ok, ct)
        }
      }
    }
  }

  test("GET shopping cart failure") {
    forAll { ct: CartTotal =>
      IOAssertion {
        GET(Uri.unsafeFromString("/cart")).flatMap { req =>
          val routes = new CartRoutes[IO](failingCart(ct)).routes(authMiddleWare)
          assertHttpFailure(routes, req)
        }
      }
    }
  }

  test("POST add item to cart") {
    forAll { cart: Cart =>
      IOAssertion {
        POST(cart, Uri.unsafeFromString("/cart")).flatMap { req =>
          val routes = new CartRoutes[IO](new TestCart).routes(authMiddleWare)
          assertHttpStatus(routes, req)(Status.Created)
        }
      }
    }
  }

  test("PUT update items in cart") {
    forAll { cart: Cart =>
      IOAssertion {
        PUT(cart, Uri.unsafeFromString("/cart")).flatMap { req =>
          val routes = new CartRoutes[IO](new TestCart).routes(authMiddleWare)
          assertHttpStatus(routes, req)(Status.Ok)
        }
      }
    }
  }

  test("DELETE item in cart") {
    forAll { id: ItemId =>
      IOAssertion {
        DELETE(Uri.unsafeFromString(s"/cart/${id.value}")).flatMap { req =>
          val routes = new CartRoutes[IO](new TestCart).routes(authMiddleWare)
          assertHttpStatus(routes, req)(Status.NoContent)
        }
      }
    }
  }
}

protected class TestCart extends ShoppingCart[IO] {
  override def add(userId: auth.UserId, itemId: item.ItemId, quantity: Quantity): IO[Unit] = IO.unit

  override def delete(userId: auth.UserId): IO[Unit] = IO.unit

  override def get(userId: auth.UserId): IO[CartTotal] =
    IO.pure(CartTotal(List.empty, USD(0)))

  override def removeItem(userId: auth.UserId, itemId: item.ItemId): IO[Unit] = IO.unit

  override def update(userId: auth.UserId, cart: Cart): IO[Unit] = IO.unit
}
