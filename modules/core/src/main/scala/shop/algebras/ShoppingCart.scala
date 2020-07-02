package shop.algebras

import cats.effect.Sync
import cats.implicits._
import dev.profunktor.redis4cats.RedisCommands
import shop.config.data.ShoppingCartExpiration
import shop.domain.auth.UserId
import shop.domain.item.ItemId
import shop.domain.shoppingCart.{ Cart, CartItem, CartTotal, Quantity }
import shop.effects._
import squants.market.{ Money, USD }

trait ShoppingCart[F[_]] {
  def add(
      userId: UserId,
      itemId: ItemId,
      quantity: Quantity
  ): F[Unit]

  def delete(userId: UserId): F[Unit]
  def get(userId: UserId): F[CartTotal]
  def removeItem(userId: UserId, itemId: ItemId): F[Unit]
  def update(userId: UserId, cart: Cart): F[Unit]
}

final class LiveShoppingCart[F[_]: MonadThrow: GenUUID] private (
    items: Items[F],
    redis: RedisCommands[F, String, String],
    exp: ShoppingCartExpiration
) extends ShoppingCart[F] {
  override def add(userId: UserId, itemId: ItemId, quantity: Quantity): F[Unit] =
    redis.hSet(
      userId.value.toString,
      itemId.value.toString,
      quantity.value.toString
    ) *> redis.expire(
          userId.value.toString,
          exp.value
        )

  override def delete(userId: UserId): F[Unit] =
    redis.del(userId.value.toString)

  override def get(userId: UserId): F[CartTotal] =
    redis.hGetAll(userId.value.toString).flatMap { it =>
      it.toList
        .traverseFilter {
          case (k, v) =>
            for {
              id <- GenUUID[F].read[ItemId](k)
              qt <- MonadThrow[F].catchNonFatal(Quantity(v.toInt))
              res <- items
                      .findById(id)
                      .map(
                        _.map(i => CartItem(i, qt))
                      )
            } yield res
        }
        .map(it => CartTotal(it, calcTotal(it)))
    }

  override def removeItem(userId: UserId, itemId: ItemId): F[Unit] =
    redis.hDel(userId.value.toString, itemId.value.toString)

  override def update(userId: UserId, cart: Cart): F[Unit] =
    redis.hGetAll(userId.value.toString).flatMap { it =>
      it.toList.traverse_ {
        case (k, _) =>
          GenUUID[F].read[ItemId](k).flatMap { id =>
            cart.items.get(id).traverse_ { q =>
              redis.hSet(userId.value.toString, id.value.toString, q.value.toString)
            }
          }
      } *> redis.expire(userId.value.toString, exp.value)
    }

  private def calcTotal(items: List[CartItem]): Money =
    USD(
      items.foldMap(i => i.item.price.amount * i.quantity.value)
    )
}

object LiveShoppingCart {
  def make[F[_]: Sync](
      items: Items[F],
      redis: RedisCommands[F, String, String],
      exp: ShoppingCartExpiration
  ): F[LiveShoppingCart[F]] = Sync[F].delay {
    new LiveShoppingCart[F](items, redis, exp)
  }
}
