package shop

import cats.effect.{ ExitCode, IO, IOApp }
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.server.blaze.BlazeServerBuilder
import shop.config.ConfigLoader
import shop.modules._

object Main extends IOApp {
  implicit val logger = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    ConfigLoader.apply[IO].flatMap { cfg =>
      Logger[IO].info(s"Loaded config $cfg") *>
        AppResources.make[IO](cfg).use { res =>
          for {
            security <- Security.make[IO](cfg, res.psql, res.redis)
            algebras <- Algebras.make[IO](res.redis, res.psql, cfg.cartExpiration)
            clients <- HttpClients.make[IO](cfg.paymentConfig, res.client)
            services <- Services.make[IO](cfg.checkoutConfig, algebras, clients)
            api <- HttpApi.make[IO](algebras, services, security)
            _ <- BlazeServerBuilder[IO]
                  .bindHttp(host = cfg.httpServerConfig.host.value, port = cfg.httpServerConfig.port.value)
                  .withHttpApp(api.httpApp)
                  .serve
                  .compile
                  .drain
          } yield ExitCode.Success
        }
    }
}
