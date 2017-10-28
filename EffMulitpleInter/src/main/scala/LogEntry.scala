import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
case class LogEntry(s: String)
object Types {
  type Result[A] = Writer[LogEntry, A]
}
