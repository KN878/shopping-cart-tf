package shop

import cats.effect.Bracket
import cats.{ ApplicativeError, MonadError }

package object effects {
  type BracketThrow[F[_]] = Bracket[F, Throwable]
  object BracketThrow {
    def apply[F[_]](implicit ev: BracketThrow[F]): BracketThrow[F] = ev
  }

  type ApThrow[F[_]] = ApplicativeError[F, Throwable]
  object ApThrow {
    def apply[F[_]](implicit ev: ApThrow[F]): ApThrow[F] = ev
  }

  type MonadThrow[F[_]] = MonadError[F, Throwable]
  object MonadThrow {
    def apply[F[_]](implicit ev: MonadThrow[F]): MonadThrow[F] = ev
  }
}
