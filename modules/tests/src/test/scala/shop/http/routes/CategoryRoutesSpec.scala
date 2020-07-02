package shop.http.routes

import cats.effect._
import org.http4s._
import org.http4s.Method._
import org.http4s.client.dsl.io._
import shop.algebras.Categories
import shop.domain.category._
import shop.arbitraries._
import shop.http.json._
import suite._

final class CategoryRoutesSpec extends HttpTestSuite {
  def dataCategories(c: List[Category]) = new TestCategories {
    override def findAll: IO[List[Category]] =
      IO.pure(c)
  }

  def failingCategories(c: List[Category]) = new TestCategories {
    override def findAll: IO[List[Category]] =
      IO.raiseError(DummyError) *> IO.pure(c)
  }

  test("successfully get categories") {
    forAll { c: List[Category] =>
      IOAssertion {
        GET(Uri.unsafeFromString("/categories")).flatMap { req =>
          val routes = new CategoryRoutes[IO](dataCategories(c)).routes
          assertHttp(routes, req)(Status.Ok, c)
        }
      }
    }
  }

  test("failed to get categories") {
    forAll { c: List[Category] =>
      IOAssertion {
        GET(Uri.unsafeFromString("/categories")).flatMap { req =>
          val routes = new CategoryRoutes[IO](failingCategories(c)).routes
          assertHttpFailure(routes, req)
        }
      }
    }
  }
}

protected class TestCategories extends Categories[IO] {
  override def findAll: IO[List[Category]] = IO.pure(List.empty)

  override def create(name: CategoryName): IO[Unit] = IO.unit
}
