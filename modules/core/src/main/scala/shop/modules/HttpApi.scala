package shop.modules

import cats.effect._
import cats.implicits._
import dev.profunktor.auth._
import dev.profunktor.auth.jwt._
import org.http4s._
import org.http4s.implicits._
import org.http4s.server._
import org.http4s.server.middleware._
import pdi.jwt._
import shop.http.auth.users._
import shop.http.routes.admin._
import shop.http.routes.auth._
import shop.http.routes.secured._
import shop.http.routes._

import scala.concurrent.duration._

final class HttpApi[F[_]: Concurrent: Timer] private (
    algebras: Algebras[F],
    services: Services[F],
    security: Security[F]
) {
  private val adminAuth: JwtToken => JwtClaim => F[Option[AdminUser]] =
    token => claim => security.adminAuth.findUser(token)(claim)

  private val userAuth: JwtToken => JwtClaim => F[Option[CommonUser]] =
    token => claim => security.userAuth.findUser(token)(claim)

  private val adminMiddleware = JwtAuthMiddleware[F, AdminUser](
    security.adminJwtAuth.value,
    adminAuth
  )

  private val userMiddleware = JwtAuthMiddleware[F, CommonUser](
    security.userJwtAuth.value,
    userAuth
  )

  // Auth routes
  private val loginRoutes        = new LoginRoutes[F](security.auth).routes
  private val logoutRoutes       = new LogoutRoutes[F](security.auth).routes(userMiddleware)
  private val registrationRoutes = new RegistrationRoutes[F](security.auth).routes

  // Open routes
  private val brandRoutes       = new BrandRoutes[F](algebras.brands).routes
  private val categoryRoutes    = new CategoryRoutes[F](algebras.categories).routes
  private val itemRoutes        = new ItemRoutes[F](algebras.items).routes
  private val healthCheckRoutes = new HealthCheckRoutes[F](algebras.healthCheck).routes

  // Secured routes
  private val cartRoutes     = new CartRoutes[F](algebras.cart).routes(userMiddleware)
  private val checkoutRoutes = new CheckoutRoutes[F](services.checkout).routes(userMiddleware)
  private val orderRoutes    = new OrderRoutes[F](algebras.orders).routes(userMiddleware)

  // Admin routes
  private val adminBrandRoutes    = new AdminBrandRoutes[F](algebras.brands).routes(adminMiddleware)
  private val adminCategoryRoutes = new AdminCategoryRoutes[F](algebras.categories).routes(adminMiddleware)
  private val adminItemRoutes     = new AdminItemRoutes[F](algebras.items).routes(adminMiddleware)

  // Combining all user routes
  private val userRoutes: HttpRoutes[F] = loginRoutes <+> registrationRoutes <+>
        brandRoutes <+> categoryRoutes <+> itemRoutes <+> healthCheckRoutes <+> cartRoutes <+>
        checkoutRoutes <+> orderRoutes <+> logoutRoutes

  // Combining all admin routes
  private val adminRoutes: HttpRoutes[F] = adminBrandRoutes <+> adminCategoryRoutes <+> adminItemRoutes

  private val routes: HttpRoutes[F] = Router(
    "v1/" -> userRoutes,
    "v1/admin/" -> adminRoutes
  )

  private val middleware: HttpRoutes[F] => HttpRoutes[F] = {
    { httpRoutes: HttpRoutes[F] =>
      AutoSlash(httpRoutes)
    } andThen { http: HttpRoutes[F] =>
      CORS(http, CORS.DefaultCORSConfig)
    } andThen { http: HttpRoutes[F] =>
      Timeout(60.seconds)(http)
    }
  }

  private val loggers: HttpApp[F] => HttpApp[F] = {
    { http: HttpApp[F] =>
      RequestLogger.httpApp(true, true)(http)
    } andThen { http: HttpApp[F] =>
      ResponseLogger.httpApp(true, true)(http)
    }
  }

  val httpApp: HttpApp[F] = loggers(middleware(routes).orNotFound)
}

object HttpApi {
  def make[F[_]: Concurrent: Timer](
      algebras: Algebras[F],
      services: Services[F],
      security: Security[F]
  ): F[HttpApi[F]] =
    Sync[F].delay {
      new HttpApi[F](algebras, services, security)
    }
}
