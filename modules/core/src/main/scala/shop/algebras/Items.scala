package shop.algebras

import cats.effect._
import cats.implicits._
import shop.domain.brand.{ Brand, _ }
import shop.domain.category._
import shop.domain.item._
import shop.effects._
import shop.extensions.skunkx._
import skunk._
import skunk.codec.all._
import skunk.implicits._
import squants.market.USD

trait Items[F[_]] {
  def findAll: F[List[Item]]
  def findBy(brand: BrandName): F[List[Item]]
  def findById(itemId: ItemId): F[Option[Item]]
  def create(item: CreateItem): F[Unit]
  def update(item: UpdateItem): F[Unit]
}

final class LiveItems[F[_]: Sync: BracketThrow: GenUUID] private (
    sessionPool: Resource[F, Session[F]]
) extends Items[F] {
  import ItemQueries._

  override def findAll: F[List[Item]] =
    sessionPool.use(_.execute(selectAll))

  override def findBy(brand: BrandName): F[List[Item]] =
    sessionPool.use { session =>
      session.prepare(selectByBrand).use { ps =>
        ps.stream(brand, 1024).compile.toList
      }
    }

  override def findById(itemId: ItemId): F[Option[Item]] =
    sessionPool.use { session =>
      session.prepare(selectById).use { ps =>
        ps.option(itemId)
      }
    }

  override def create(item: CreateItem): F[Unit] =
    sessionPool.use { session =>
      session.prepare(insertItem).use { cmd =>
        GenUUID[F].make[ItemId].flatMap { id =>
          cmd.execute(id ~ item).void
        }
      }
    }

  override def update(item: UpdateItem): F[Unit] =
    sessionPool.use { session =>
      session.prepare(updateItem).use { cmd =>
        cmd.execute(item).void
      }
    }
}

object LiveItems {
  def make[F[_]: Sync](sessionPool: Resource[F, Session[F]]): F[LiveItems[F]] =
    Sync[F].delay(new LiveItems[F](sessionPool))
}

private object ItemQueries {
  val decoder: Decoder[Item] = (
    uuid ~ varchar ~ varchar ~ numeric ~
        uuid ~ varchar ~ uuid ~ varchar
  ).map {
    case id ~ n ~ d ~ p ~ bId ~ bN ~ cId ~ cN =>
      Item(
        ItemId(id),
        ItemName(n),
        ItemDescription(d),
        USD(p),
        Brand(BrandId(bId), BrandName(bN)),
        Category(CategoryId(cId), CategoryName(cN))
      )
  }

  val selectAll: Query[Void, Item] =
    sql"""
          SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
          FROM items AS i
          INNER JOIN brands AS b ON i.brand_id = b.uuid
          INNER JOIN categories AS c ON i.category_id = c.uuid
         """.query(decoder)

  val selectByBrand: Query[BrandName, Item] =
    sql"""
         SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
          FROM items AS i
          INNER JOIN brands AS b ON i.brand_id = b.uuid
          INNER JOIN categories AS c ON i.category_id = c.uuid
          WHERE b.name LIKE ${varchar.cimap[BrandName]}
         """.query(decoder)

  val selectById: Query[ItemId, Item] =
    sql"""
         SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
          FROM items AS i
          INNER JOIN brands AS b ON i.brand_id = b.uuid
          INNER JOIN categories AS c ON i.category_id = c.uuid
          WHERE i.uuid =${uuid.cimap[ItemId]}
         """.query(decoder)

  val insertItem: Command[ItemId ~ CreateItem] =
    sql"""
         INSERT INTO items
         VALUES ($uuid, $varchar, $varchar, $numeric, $uuid, $uuid)
         """.command.contramap {
      case id ~ i =>
        id.value ~ i.name.value ~ i.description.value ~ i.price.amount ~
            i.brandId.value ~ i.categoryId.value
    }

  val updateItem: Command[UpdateItem] =
    sql"""
         UPDATE items
         SET price = $numeric
         WHERE uuid = ${uuid.cimap[ItemId]}
         """.command.contramap(i => i.price.amount ~ i.uuid)
}
