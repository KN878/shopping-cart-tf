package shop.domain

import java.util.UUID

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.{ Uuid, ValidBigDecimal }
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype
import shop.domain.brand.{ Brand, BrandId }
import shop.domain.category.{ Category, CategoryId }
import squants.market.{ Money, USD }

object item {

  @newtype case class ItemId(value: UUID)

  @newtype case class ItemName(value: String)

  @newtype case class ItemDescription(value: String)

  case class Item(
      uuid: ItemId,
      name: ItemName,
      description: ItemDescription,
      price: Money,
      brand: Brand,
      category: Category
  )

  @newtype case class ItemNameParam(value: NonEmptyString)
  @newtype case class ItemDescriptionParam(value: NonEmptyString)
  @newtype case class ItemPriceParam(value: String Refined ValidBigDecimal)

  case class CreateItemParam(
      name: ItemNameParam,
      description: ItemDescriptionParam,
      price: ItemPriceParam,
      brandId: BrandId,
      categoryId: CategoryId
  ) {
    def toDomain: CreateItem =
      CreateItem(
        ItemName(name.value.value),
        ItemDescription(description.value.value),
        USD(BigDecimal(price.value.value)),
        brandId,
        categoryId
      )
  }

  case class CreateItem(
      name: ItemName,
      description: ItemDescription,
      price: Money,
      brandId: BrandId,
      categoryId: CategoryId
  )

  @newtype case class ItemIdParam(value: String Refined Uuid)

  case class UpdateItemParam(
      itemId: ItemIdParam,
      price: ItemPriceParam
  ) {
    def toDomain: UpdateItem =
      UpdateItem(
        ItemId(UUID.fromString(itemId.value.value)),
        USD(BigDecimal(price.value.value))
      )
  }

  case class UpdateItem(
      uuid: ItemId,
      price: Money
  )

}
