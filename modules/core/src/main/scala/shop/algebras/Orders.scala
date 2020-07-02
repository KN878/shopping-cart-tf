package shop.algebras

import cats.effect.{ Resource, Sync }
import cats.implicits._
import shop.domain.auth._
import shop.domain.item.ItemId
import shop.domain.order._
import shop.domain.shoppingCart._
import shop.effects._
import shop.extensions.skunkx._
import skunk._
import skunk.circe.codec.all._
import skunk.codec.all._
import skunk.implicits._
import squants.market._
import shop.http.json._

trait Orders[F[_]] {
  def get(
      userId: UserId,
      orderId: OrderId
  ): F[Option[Order]]

  def findBy(userId: UserId): F[List[Order]]
  def create(
      userId: UserId,
      pid: PaymentId,
      items: List[CartItem],
      total: Money
  ): F[OrderId]
}

final class LiveOrders[F[_]: Sync: BracketThrow: GenUUID] private (
    sessionPool: Resource[F, Session[F]]
) extends Orders[F] {
  import OrderQueries._

  override def get(userId: UserId, orderId: OrderId): F[Option[Order]] =
    sessionPool.use(
      s =>
        s.prepare(selectByUserIdAndOrderId).use { ps =>
          ps.option(userId ~ orderId)
        }
    )

  override def findBy(userId: UserId): F[List[Order]] =
    sessionPool.use { s =>
      s.prepare(selectByUserId).use { ps =>
        ps.stream(userId, 1024).compile.toList
      }
    }

  override def create(userId: UserId, pid: PaymentId, items: List[CartItem], total: Money): F[OrderId] =
    sessionPool.use { s =>
      s.prepare(insertOrder).use { cmd =>
        GenUUID[F].make[OrderId].flatMap { oId =>
          val order = Order(oId, pid, items.map(i => i.item.uuid -> i.quantity).toMap, total)
          cmd.execute(userId ~ order).as(oId)
        }
      }
    }
}

object LiveOrders {
  def make[F[_]: Sync](sessionPool: Resource[F, Session[F]]) =
    Sync[F].delay(new LiveOrders[F](sessionPool))
}

private object OrderQueries {
  val encoder: Encoder[UserId ~ Order] =
    (uuid.cimap[OrderId] ~ uuid.cimap[UserId] ~ uuid.cimap[PaymentId] ~
        jsonb[Map[ItemId, Quantity]] ~ numeric.contramap[Money](_.amount)).contramap {
      case id ~ o =>
        o.id ~ id ~ o.pid ~ o.items ~ o.total
    }

  val decoder: Decoder[Order] =
    (uuid.cimap[OrderId] ~ uuid ~ uuid.cimap[PaymentId] ~
        jsonb[Map[ItemId, Quantity]] ~ numeric.map(USD(_))).map {
      case id ~ _ ~ pid ~ items ~ total => Order(id, pid, items, total)
    }

  val selectByUserId: Query[UserId, Order] =
    sql"""
         SELECT *
         FROM orders
         WHERE user_id = ${uuid.cimap[UserId]}
         """.query(decoder)

  val selectByUserIdAndOrderId: Query[UserId ~ OrderId, Order] =
    sql"""
         SELECT *
         FROM orders
         WHERE user_id = ${uuid.cimap[UserId]}
         AND uuid = ${uuid.cimap[OrderId]}
         """.query(decoder)

  val insertOrder: Command[UserId ~ Order] =
    sql"""
          INSERT INTO orders
          VALUES ($encoder)
         """.command

}
