package shop.resources

import cats.effect._
import cats.implicits.{ catsSyntaxEq => _, _ }
import ciris._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import natchez.Trace.Implicits.noop
import shop.algebras._
import shop.arbitraries._
import shop.config.data.PasswordSalt
import shop.domain.auth.{ Password, UserName }
import shop.domain.brand._
import shop.domain.category.{ Category, CategoryId }
import shop.domain.item.{ CreateItem, Item }
import shop.domain.order.{ OrderId, PaymentId }
import shop.domain.shoppingCart.CartItem
import skunk._
import squants.market.Money
import suite._

final class PostgresTest extends ResourceTestSuite[Resource[IO, Session[IO]]] {
  val MaxTests: PropertyCheckConfigParam = MinSuccessful(1)

  lazy val salt = PasswordSalt(Secret("53kr3t": NonEmptyString))

  override def resources: Resource[IO, Resource[IO, Session[IO]]] =
    Session.pooled[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      database = "store",
      password = Some("qwer123"),
      max = 10
    )

  withResources { pool =>
    test("Brands") {
      forAll(MaxTests) { b: Brand =>
        IOAssertion {
          LiveBrands.make[IO](pool).flatMap { brands =>
            for {
              x <- brands.findAll
              _ <- brands.create(b.name)
              y <- brands.findAll
              z <- brands.create(b.name).attempt
            } yield assert(
              x.isEmpty &&
                y.count(_.name === b.name) === 1 &&
                z.isLeft
            )
          }
        }
      }
    }

    test("Categories") {
      forAll(MaxTests) { c: Category =>
        IOAssertion {
          LiveCategories.make[IO](pool).flatMap { categories =>
            for {
              x <- categories.findAll
              _ <- categories.create(c.name)
              y <- categories.findAll
              z <- categories.create(c.name).attempt
            } yield assert(
              x.isEmpty &&
                y.count(_.name === c.name) === 1 &&
                z.isLeft
            )
          }
        }
      }
    }

    test("Items") {
      forAll(MaxTests) { item: Item =>
        def newItem(bid: Option[BrandId], cid: Option[CategoryId]) =
          CreateItem(
            item.name,
            item.description,
            item.price,
            bid.getOrElse(item.brand.uuid),
            cid.getOrElse(item.category.uuid)
          )

        IOAssertion {
          for {
            items <- LiveItems.make[IO](pool)
            brands <- LiveBrands.make[IO](pool)
            cats <- LiveCategories.make[IO](pool)
            x <- items.findAll
            _ <- brands.create(item.brand.name)
            bid <- brands.findAll.map(_.headOption.map(_.uuid))
            _ <- cats.create(item.category.name)
            cid <- cats.findAll.map(_.headOption.map(_.uuid))
            _ <- items.create(newItem(bid, cid))
            z <- items.findAll
          } yield assert(
            x.isEmpty &&
              z.count(_.name === item.name) === 1
          )
        }
      }
    }

    test("Users") {
      forAll(MaxTests) { (username: UserName, password: Password) =>
        IOAssertion {
          for {
            crypto <- LiveCrypto.make[IO](salt)
            users <- LiveUsers.make[IO](pool, crypto)
            x <- users.create(username, password)
            y <- users.find(username, password)
            z <- users.find(username, Password("foo"))
            v <- users.create(username, password).attempt
          } yield assert(
            y.count(_.id === x) === 1 &&
              z.isEmpty &&
              v.isLeft
          )
        }
      }
    }

    test("Orders") {
      forAll(MaxTests) {
        (oid: OrderId, pid: PaymentId, username: UserName, pass: Password, items: List[CartItem], price: Money) =>
          for {
            orders <- LiveOrders.make[IO](pool)
            crypto <- LiveCrypto.make[IO](salt)
            users <- LiveUsers.make[IO](pool, crypto)
            uId <- users.create(username, pass)
            x <- orders.findBy(uId)
            y <- orders.get(uId, oid)
            z <- orders.create(uId, pid, items, price)
          } yield assert(
            x.isEmpty && y.isEmpty &&
              z.value.version() === 4 // random uuid
          )
      }
    }
  }
}
