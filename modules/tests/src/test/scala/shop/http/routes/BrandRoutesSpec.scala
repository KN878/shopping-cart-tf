package shop.http.routes

import cats.effect._
import org.http4s._
import org.http4s.Method._
import org.http4s.client.dsl.io._
import shop.algebras.Brands
import shop.domain.brand._
import shop.arbitraries._
import shop.http.json._
import suite._

final class BrandRoutesSpec extends HttpTestSuite {
  def dataBrands(brands: List[Brand]) = new TestBrands {
    override def findAll: IO[List[Brand]] = IO.pure(brands)
  }

  def failingBrands(brands: List[Brand]) = new TestBrands {
    override def findAll: IO[List[Brand]] =
      IO.raiseError(DummyError) *> IO.pure(brands)
  }

  test("successfully get all brands") {
    forAll { b: List[Brand] =>
      IOAssertion {
        GET(Uri.unsafeFromString("/brands")).flatMap { req =>
          val routes = new BrandRoutes[IO](dataBrands(b)).routes
          assertHttp(routes, req)(Status.Ok, b)
        }
      }
    }
  }

  test("failed to get all brands") {
    forAll { b: List[Brand] =>
      IOAssertion {
        GET(Uri.unsafeFromString("/brands")).flatMap { req =>
          val routes = new BrandRoutes[IO](failingBrands(b)).routes
          assertHttpFailure(routes, req)
        }
      }
    }
  }
}

protected class TestBrands extends Brands[IO] {
  override def findAll: IO[List[Brand]] = IO.pure(List.empty)

  override def create(name: BrandName): IO[Unit] = IO.unit
}
