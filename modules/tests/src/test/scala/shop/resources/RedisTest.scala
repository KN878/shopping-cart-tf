package shop.resources

import java.util.UUID

import cats.Eq
import cats.effect._
import cats.effect.concurrent._
import cats.implicits.{ catsSyntaxEq => _, _ }
import ciris.Secret
import dev.profunktor.auth.jwt._
import dev.profunktor.redis4cats._
import dev.profunktor.redis4cats.log4cats._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import pdi.jwt._
import shop.algebras.{ Items, LiveAuth, LiveShoppingCart, LiveTokens, LiveUsersAuth, Users }
import shop.arbitraries._
import shop.config.data._
import shop.domain.auth._
import shop.domain.brand._
import shop.domain.category._
import shop.domain.item._
import shop.domain.shoppingCart._
import shop.effects.GenUUID
import shop.http.auth.users._
import shop.logger.NoOp
import suite._

import scala.concurrent.duration._

final class RedisTest extends ResourceTestSuite[RedisCommands[IO, String, String]] {
  val MaxTests: PropertyCheckConfigParam = MinSuccessful(1)

  lazy val cartExp     = ShoppingCartExpiration(30.seconds)
  lazy val tokenConfig = JwtSecretKeyConfig(Secret("bar": NonEmptyString))
  lazy val tokenExp    = TokenExpiration(30.seconds)
  lazy val claim       = JwtClaim("test")
  lazy val userJwtAuth = UserJwtAuth(JwtAuth.hmac("bar", JwtAlgorithm.HS256))

  override def resources: Resource[IO, RedisCommands[IO, String, String]] =
    Redis[IO].utf8("redis://localhost")

  withResources { cmd =>
    test("Shopping Cart") {
      forAll(MaxTests) { (uid: UserId, it1: Item, it2: Item, q1: Quantity, q2: Quantity) =>
        IOAssertion {
          Ref
            .of[IO, Map[ItemId, Item]](
              Map((it1.uuid -> it1), (it2.uuid -> it2))
            )
            .flatMap { itemsRef =>
              val items = new TestItems(itemsRef)
              LiveShoppingCart.make[IO](items, cmd, cartExp).flatMap { cart =>
                for {
                  x <- cart.get(uid)
                  _ <- cart.add(uid, it1.uuid, q1)
                  _ <- cart.add(uid, it2.uuid, q1)
                  y <- cart.get(uid)
                  _ <- cart.removeItem(uid, it2.uuid)
                  z <- cart.get(uid)
                  _ <- cart.update(uid, Cart(Map(it1.uuid -> q2)))
                  w <- cart.get(uid)
                  _ <- cart.delete(uid)
                  v <- cart.get(uid)
                } yield assert(
                  x.items.isEmpty &&
                    y.items.size === 2 &&
                    z.items.size === 1 &&
                    v.items.isEmpty &&
                    w.items.headOption.fold(false)(_.quantity === q2)
                )
              }
            }
        }
      }
    }

    test("Authentication") {
      forAll(MaxTests) { (un1: UserName, un2: UserName, pw: Password) =>
        IOAssertion {
          for {
            tokens <- LiveTokens.make[IO](tokenConfig, tokenExp)
            auth <- LiveAuth.make[IO](tokenExp, tokens, new TestUsers(un2), cmd)
            users <- LiveUsersAuth.make[IO](cmd)
            x <- users.findUser(JwtToken("invalid"))(claim)
            j <- auth.newUser(un1, pw)
            e <- jwtDecode[IO](j, userJwtAuth.value).attempt
            k <- auth.login(un2, pw)
            f <- jwtDecode[IO](k, userJwtAuth.value).attempt
            _ <- auth.logout(k, un2)
            y <- users.findUser(k)(claim)
            z <- users.findUser(j)(claim)
          } yield assert(
            x.isEmpty && e.isRight && f.isRight && y.isEmpty &&
              z.fold(false)(_.value.name === un1)
          )
        }
      }
    }
  }
}

protected class TestUsers(un: UserName) extends Users[IO] {
  override def find(userName: UserName, password: Password): IO[Option[User]] =
    (Eq[UserName]
      .eqv(un, userName))
      .guard[Option]
      .as(User(UserId(UUID.randomUUID()), un))
      .pure[IO]

  override def create(userName: UserName, password: Password): IO[UserId] =
    GenUUID[IO].make[UserId]
}

protected class TestItems(ref: Ref[IO, Map[ItemId, Item]]) extends Items[IO] {
  override def findAll: IO[List[Item]] =
    ref.get.map(_.values.toList)

  override def findBy(brand: BrandName): IO[List[Item]] =
    IO.pure(List.empty)

  override def findById(itemId: ItemId): IO[Option[Item]] =
    ref.get.map(_.get(itemId))

  override def create(item: CreateItem): IO[Unit] =
    GenUUID[IO].make[ItemId].flatMap { id =>
      val brand    = Brand(item.brandId, BrandName("foo"))
      val category = Category(item.categoryId, CategoryName("bar"))
      val newItem  = Item(id, item.name, item.description, item.price, brand, category)
      ref.update(_.updated(id, newItem))
    }

  override def update(item: UpdateItem): IO[Unit] =
    ref.update { m =>
      m.get(item.uuid)
        .fold(m) { i =>
          m.updated(i.uuid, i.copy(price = item.price))
        }
    }
}
