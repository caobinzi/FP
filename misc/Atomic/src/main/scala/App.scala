import scala.language.higherKinds
import java.util.concurrent.atomic._
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._
import scala.concurrent._
import ExecutionContext.Implicits.global

object AtomicApp extends App {
  val ref = new AtomicInteger(1)
  println(ref.incrementAndGet)
  println(ref.incrementAndGet)
  println(ref.incrementAndGet)
}
