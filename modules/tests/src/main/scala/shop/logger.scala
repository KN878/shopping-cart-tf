package shop

import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.log4cats.Logger

object logger {
  implicit object NoOp extends NoLogger

  private[logger] class NoLogger extends Logger[IO] {
    override def error(message: => String): IO[Unit] = IO.unit

    override def warn(message: => String): IO[Unit] = IO.unit

    override def info(message: => String): IO[Unit] = IO.unit

    override def debug(message: => String): IO[Unit] = IO.unit

    override def trace(message: => String): IO[Unit] = IO.unit

    override def error(t: Throwable)(message: => String): IO[Unit] = IO.unit

    override def warn(t: Throwable)(message: => String): IO[Unit] = IO.unit

    override def info(t: Throwable)(message: => String): IO[Unit] = IO.unit

    override def debug(t: Throwable)(message: => String): IO[Unit] = IO.unit

    override def trace(t: Throwable)(message: => String): IO[Unit] = IO.unit
  }

  def acc(ref: Ref[IO, List[String]]): Logger[IO] =
    new NoLogger {
      override def error(message: => String): IO[Unit] =
        ref.update(xs => message :: xs)
    }
}
