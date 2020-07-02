package shop

import java.util.UUID

import eu.timepit.refined.api.Refined
import io.estatico.newtype.Coercible
import io.estatico.newtype.ops._
import org.scalacheck._
import shop.domain.auth.{ Password, UserName }
import shop.domain.brand._
import shop.domain.category._
import shop.domain.checkout._
import shop.domain.item._
import shop.domain.shoppingCart._
import squants.market.{ Money, USD }

object generators {
  def cbUuid[A: Coercible[UUID, *]]: Gen[A] = Gen.uuid.map(_.coerce[A])

  def cbStr[A: Coercible[String, *]]: Gen[A] = genNonEmptyString.map(_.coerce[A])

  def cbInt[A: Coercible[Int, *]]: Gen[A] = Gen.posNum[Int].map(_.coerce[A])

  val genNonEmptyString: Gen[String] =
    Gen
      .chooseNum(10, 30)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }

  val usernameGen: Gen[UserName] = for {
    name <- genNonEmptyString
  } yield UserName(name)

  val passwordGen: Gen[Password] = for {
    password <- genNonEmptyString
  } yield Password(password)

  val genMoney: Gen[Money] =
    Gen.posNum[Long].map { m =>
      USD(BigDecimal(m))
    }

  val brandGen: Gen[Brand] = for {
    id <- cbUuid[BrandId]
    name <- cbStr[BrandName]
  } yield Brand(id, name)

  val categoryGen: Gen[Category] = for {
    id <- cbUuid[CategoryId]
    name <- cbStr[CategoryName]
  } yield Category(id, name)

  val itemGen: Gen[Item] = for {
    id <- cbUuid[ItemId]
    iName <- cbStr[ItemName]
    iDescr <- cbStr[ItemDescription]
    price <- genMoney
    brand <- brandGen
    category <- categoryGen
  } yield Item(id, iName, iDescr, price, brand, category)

  val cartItemGen: Gen[CartItem] = for {
    item <- itemGen
    q <- cbInt[Quantity]
  } yield CartItem(item, q)

  val cartTotalGen: Gen[CartTotal] = for {
    items <- Gen.nonEmptyListOf(cartItemGen)
    total <- genMoney
  } yield CartTotal(items, total)

  val cardGen: Gen[Card] = for {
    holder <- genNonEmptyString.map[CardHolderPred](Refined.unsafeApply)
    number <- Gen.posNum[Long].map[CardNumberPred](Refined.unsafeApply)
    exp <- Gen.posNum[Int].map[CardExpirationPred](i => Refined.unsafeApply(i.toString))
    ccv <- Gen.posNum[Int].map[CardCVVPred](Refined.unsafeApply)
  } yield Card(CardHolder(holder), CardNumber(number), CardExpiration(exp), CardCVV(ccv))

  val cartGen: Gen[Cart] = for {
    items <- Gen.nonEmptyMap(cartItemGen.map(it => (it.item.uuid, it.quantity)))
  } yield Cart(items)

}
