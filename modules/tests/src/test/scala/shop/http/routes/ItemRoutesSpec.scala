package shop.http.routes

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.Method._
import org.http4s.client.dsl.io._
import shop.algebras.Items
import shop.arbitraries._
import shop.domain.item._
import shop.domain.brand._
import suite._
import shop.http.json._

final class ItemRoutesSpec extends HttpTestSuite {
  def dataItems(it: List[Item]) = new TestItems {
    override def findAll: IO[List[Item]] = IO.pure(it)
  }

  def failingItems(it: List[Item]) = new TestItems {
    override def findAll: IO[List[Item]] =
      IO.raiseError(DummyError) *> IO.pure(it)
  }

  test("successfully get items") {
    forAll { it: List[Item] =>
      IOAssertion {
        GET(Uri.unsafeFromString("/items")).flatMap { req =>
          val routes = new ItemRoutes[IO](dataItems(it)).routes
          assertHttp(routes, req)(Status.Ok, it)
        }
      }
    }
  }

  test("successfully get items by brand") {
    forAll { (it: List[Item], b: Brand) =>
      IOAssertion {
        GET(Uri.unsafeFromString("/items").withQueryParam(b.name.value)).flatMap { req =>
          val routes = new ItemRoutes[IO](dataItems(it)).routes
          assertHttp(routes, req)(Status.Ok, it)
        }
      }
    }
  }

  test("failed to get items") {
    forAll { it: List[Item] =>
      IOAssertion {
        GET(Uri.unsafeFromString("/items")).flatMap { req =>
          val routes = new ItemRoutes[IO](failingItems(it)).routes
          assertHttpFailure(routes, req)
        }
      }
    }
  }
}

protected class TestItems extends Items[IO] {
  override def findAll: IO[List[Item]] = IO.pure(List.empty)

  override def findBy(brand: BrandName): IO[List[Item]] = IO.pure(List.empty)

  override def findById(itemId: ItemId): IO[Option[Item]] = IO.pure(none[Item])

  override def create(item: CreateItem): IO[Unit] = IO.unit

  override def update(item: UpdateItem): IO[Unit] = IO.unit
}
