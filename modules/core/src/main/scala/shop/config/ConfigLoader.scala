package shop.config

import cats.effect._
import cats.implicits._
import ciris._
import ciris.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.cats._
import shop.config.data._
import shop.config.environments.AppEnvironment
import shop.config.environments.AppEnvironment._

import scala.concurrent.duration._

object ConfigLoader {
  def apply[F[_]: Async: ContextShift]: F[AppConfig] =
    env("SC_APP_ENV")
      .as[AppEnvironment]
      .flatMap {
        case Test =>
          default(
            redisURI = RedisURI("redis://localhost"),
            paymentURI = PaymentURI("http://dummy_payments.net/api")
          )
        case Prod =>
          default(
            redisURI = RedisURI("redis://10.123.145.167"),
            paymentURI = PaymentURI("https://real_payments.net/api")
          )
      }
      .load[F]

  private def default(
      redisURI: RedisURI,
      paymentURI: PaymentURI
  ): ConfigValue[AppConfig] =
    (
      env("SC_JWT_SECRET_KEY").as[NonEmptyString].secret,
      env("SC_JWT_CLAIM").as[NonEmptyString].secret,
      env("SC_ACCESS_TOKEN_SECRET_KEY").as[NonEmptyString].secret,
      env("SC_ADMIN_USER_TOKEN").as[NonEmptyString].secret,
      env("SC_PASSWORD_SALT").as[NonEmptyString].secret
    ).parMapN { (secretKey, jwtClaim, tokenKey, adminToken, salt) =>
      AppConfig(
        AdminJwtConfig(
          JwtSecretKeyConfig(secretKey),
          JwtClaimConfig(jwtClaim),
          AdminUserTokenConfig(adminToken)
        ),
        JwtSecretKeyConfig(tokenKey),
        PasswordSalt(salt),
        TokenExpiration(30.minutes),
        ShoppingCartExpiration(30.minutes),
        CheckoutConfig(
          retriesLimit = 3,
          retriesBackoff = 10.milliseconds
        ),
        PaymentConfig(paymentURI),
        HttpClientConfig(
          connectTimeout = 2.seconds,
          requestTimeout = 2.seconds
        ),
        PostgreSQLConfig(
          host = "localhost",
          port = 5445,
          user = "postgres",
          database = "store",
          password = None,
          max = 10
        ),
        RedisConfig(redisURI),
        HttpServerConfig(
          host = "0.0.0.0",
          port = 8080
        )
      )
    }
}
