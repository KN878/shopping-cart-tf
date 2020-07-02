package shop.domain

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.Size
import eu.timepit.refined.string.{ MatchesRegex, ValidInt }
import io.estatico.newtype.macros.newtype

object checkout {
  type Rgx = W.`"^[a-zA-Z]+(([',. -][a-zA-Z ])?[a-zA-Z]*)*$"`.T

  type CardHolderPred     = String Refined MatchesRegex[Rgx]
  type CardNumberPred     = Long Refined Size[16]
  type CardExpirationPred = String Refined (Size[4] And ValidInt)
  type CardCVVPred        = Int Refined Size[3]

  @newtype case class CardHolder(value: CardHolderPred)
  @newtype case class CardNumber(value: CardNumberPred)
  @newtype case class CardExpiration(value: CardExpirationPred)
  @newtype case class CardCVV(value: CardCVVPred)

  case class Card(
      holder: CardHolder,
      number: CardNumber,
      expiration: CardExpiration,
      cvv: CardCVV
  )
}
