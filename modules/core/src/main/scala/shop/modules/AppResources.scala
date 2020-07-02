package shop.modules

import cats._
import cats.effect._
import cats.implicits._
import dev.profunktor.redis4cats._
import dev.profunktor.redis4cats.log4cats._
import io.chrisdavenport.log4cats.Logger
import natchez.Trace.Implicits.noop
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import shop.config.data._
import skunk._

import scala.concurrent.ExecutionContext

final case class AppResources[F[_]] private (
    client: Client[F],
    redis: RedisCommands[F, String, String],
    psql: Resource[F, Session[F]]
)

object AppResources {
  def make[F[_]: ConcurrentEffect: Parallel: ContextShift: Logger](
      cfg: AppConfig
  ): Resource[F, AppResources[F]] = {
    def mkPostgres(
        psqlCfg: PostgreSQLConfig
    ): SessionPool[F] =
      Session.pooled[F](
        host = psqlCfg.host.value,
        port = psqlCfg.port.value,
        user = psqlCfg.user.value,
        database = psqlCfg.database.value,
        password = psqlCfg.password,
        max = psqlCfg.max.value
      )

    def mkRedis(
        redisCfg: RedisConfig
    ): Resource[F, RedisCommands[F, String, String]] =
      Redis[F].utf8(redisCfg.uri.value.value)

    def mkHttpClient(
        httpClientConfig: HttpClientConfig
    ): Resource[F, Client[F]] =
      BlazeClientBuilder[F](ExecutionContext.global)
        .withConnectTimeout(httpClientConfig.connectTimeout)
        .withRequestTimeout(httpClientConfig.requestTimeout)
        .resource

    (mkHttpClient(cfg.httpClientConfig), mkRedis(cfg.redis), mkPostgres(cfg.postgreSQL))
      .parMapN(AppResources.apply[F])
  }
}
