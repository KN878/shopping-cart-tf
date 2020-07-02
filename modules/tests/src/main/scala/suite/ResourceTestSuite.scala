package suite

import cats.effect._
import cats.effect.concurrent.Deferred
import org.scalatest.BeforeAndAfterAll

trait ResourceTestSuite[A] extends PureTestSuite with BeforeAndAfterAll {

  def resources: Resource[IO, A]

  private var res: A            = _
  private var cleanUp: IO[Unit] = _
  private val latch             = Deferred[IO, Unit].unsafeRunSync()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val (r, h) = resources.allocated.unsafeRunSync()
    res = r
    cleanUp = h
    latch.complete(()).unsafeRunSync()
  }

  override protected def afterAll(): Unit = {
    cleanUp.unsafeRunSync()
    super.afterAll()
  }

  def withResources(f: (=> A) => Unit): Unit = f {
    latch.get.unsafeRunSync()
    res
  }

}
