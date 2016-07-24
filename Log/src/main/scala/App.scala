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
object WriterTLog {
  def calc1: WriterT[Option, List[String], Int] = WriterT((List("doing calc") -> 11).point[Option])
  def calc2: WriterT[Option, List[String], Int] = WriterT((List("doing other") -> 22).point[Option])

  val r = for {
    a <- calc1
    b <- calc2
  } yield {
    a + b
  }
  def log = println(r.run)
}

object OptionTWriterLog {

  type Logger[A] = WriterT[scalaz.Id.Id, List[String], A]

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

object LogApp extends App {
  WriterLog.log
  WriterTLog.log
  OptionTWriterLog.log
}

