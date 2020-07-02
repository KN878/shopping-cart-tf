package shop

import java.util.UUID

import io.estatico.newtype.Coercible
import org.scalacheck._
import shop.domain.auth.{ Password, UserName }
import shop.domain.brand.Brand
import shop.domain.category.Category
import shop.domain.checkout.Card
import shop.domain.item.Item
import shop.domain.shoppingCart._
import shop.generators._
import squants.market.Money

object arbitraries {
  implicit def arbCoercibleUUID[A: Coercible[UUID, *]]: Arbitrary[A] = Arbitrary(cbUuid[A])
  implicit def arbCoercibleInt[A: Coercible[Int, *]]: Arbitrary[A]   = Arbitrary(cbInt[A])

  implicit val arbCartTotal: Arbitrary[CartTotal] = Arbitrary(cartTotalGen)
  implicit val arbBrand: Arbitrary[Brand]         = Arbitrary(brandGen)
  implicit val arbCategory: Arbitrary[Category]   = Arbitrary(categoryGen)
  implicit val arbItem: Arbitrary[Item]           = Arbitrary(itemGen)
  implicit val arbCartItem: Arbitrary[CartItem]   = Arbitrary(cartItemGen)
  implicit val arbCard: Arbitrary[Card]           = Arbitrary(cardGen)
  implicit val arbCart: Arbitrary[Cart]           = Arbitrary(cartGen)
  implicit val arbUsername: Arbitrary[UserName]   = Arbitrary(usernameGen)
  implicit val arbPassword: Arbitrary[Password]   = Arbitrary(passwordGen)
  implicit val arbMoney: Arbitrary[Money]         = Arbitrary(genMoney)
}
