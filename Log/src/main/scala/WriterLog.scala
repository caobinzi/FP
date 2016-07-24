import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import scalaz.Scalaz._

object WriterLog {
  def calc1: Writer[List[String], Int] = Writer(List("doing calc"), 11)
  def calc2: Writer[List[String], Int] = Writer(List("doing other"), 22)

  val r = for {
    a <- calc1
    b <- calc2
  } yield {
    a + b
  }

  def log = r.run
}

