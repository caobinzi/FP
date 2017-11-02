import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import scalaz.Scalaz._


object OptionWriterLog {

  type Logger[A] = Writer[List[String], A]

  def calc1: OptionT[Logger, Int] = OptionT[Logger, Int](Writer(List("doing calc"), Some(11): Option[Int]))
  def calc2: OptionT[Logger, Int] = OptionT[Logger, Int](Writer(List("doing other"), None: Option[Int]))

  val r = for {
    a <- calc1
    b <- calc2
  } yield {
    a + b
  }
  def log = println(r.run.run)
}



