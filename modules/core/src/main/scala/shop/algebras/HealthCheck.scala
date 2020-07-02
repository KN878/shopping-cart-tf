package shop.algebras

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import dev.profunktor.redis4cats.RedisCommands
import shop.domain.healthcheck.{ AppStatus, PostgresStatus, RedisStatus }
import skunk._
import skunk.codec.all._
import skunk.implicits._

import scala.concurrent.duration._

trait HealthCheck[F[_]] {
  def status: F[AppStatus]
}

final class LiveHealthCheck[F[_]: Concurrent: Parallel: Timer] private (
    redis: RedisCommands[F, String, String],
    sessionPool: Resource[F, Session[F]]
) extends HealthCheck[F] {
  override def status: F[AppStatus] =
    (redisHealth, pgHealth).parMapN(AppStatus)

  private val queryCheck: Query[Void, Int] =
    sql"SELECT pid FROM pg_stat_activity".query(int4)

  private val pgHealth: F[PostgresStatus] =
    sessionPool
      .use(_.execute(queryCheck))
      .map(_.nonEmpty)
      .timeout(1.second)
      .orElse(false.pure[F])
      .map(PostgresStatus.apply)

  private val redisHealth: F[RedisStatus] =
    redis.ping
      .map(_.nonEmpty)
      .timeout(1.second)
      .orElse(false.pure[F])
      .map(RedisStatus.apply)
}

object LiveHealthCheck {
  def make[F[_]: Concurrent: Parallel: Timer](
      redis: RedisCommands[F, String, String],
      sessionPool: Resource[F, Session[F]]
  ): F[LiveHealthCheck[F]] =
    Sync[F].delay {
      new LiveHealthCheck[F](redis, sessionPool)
    }
}
