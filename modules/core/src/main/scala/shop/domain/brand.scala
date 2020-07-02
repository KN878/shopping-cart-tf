package shop.domain

import java.util.UUID

import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype

import scala.util.control.NoStackTrace

object brand {
  @newtype case class BrandId(value: UUID)
  @newtype case class BrandName(value: String)

  case class Brand(uuid: BrandId, name: BrandName)

  @newtype case class BrandParam(value: NonEmptyString) {
    def toDomain: BrandName = BrandName(value.value.toLowerCase.capitalize)
  }
  case class InvalidBrand(value: String) extends NoStackTrace
}
