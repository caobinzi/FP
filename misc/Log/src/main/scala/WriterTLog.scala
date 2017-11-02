import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import scalaz.Scalaz._

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


