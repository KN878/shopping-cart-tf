package shop.http.routes

import cats.{ Defer, Monad }
import org.http4s._
import org.http4s.dsl.Http4sDsl
import shop.algebras.Brands
import org.http4s.server.Router
import shop.http.json._

final class BrandRoutes[F[_]: Defer: Monad](
    brands: Brands[F]
) extends Http4sDsl[F] {

  private[routes] val prefixPath = "/brands"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok(brands.findAll)
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
