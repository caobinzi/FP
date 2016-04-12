import scalaz._
import scalaz.Scalaz._
import Helper._
sealed trait Log[A]
object Log {
  case class Info(txt: String) extends Log[Unit]
  case class Warn(txt: String) extends Log[Unit]
}
object Printer extends (Log ~> Id) {
  import Log._
  def apply[A](l: Log[A]): Id[A] = l match {
    case Info(txt) => println(txt)
    case Warn(txt) => System.err.println(txt)
  }
}

