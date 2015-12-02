 package free.task
import scalaz._
import scalaz.Scalaz._
import free._
object Printer extends (Log ~> Id) {
    import Log._
    def apply[A](l: Log[A]): Id[A] = l match {
      case Info(txt) => println(txt)
      case Warn(txt) => System.err.println(txt)
    }
  }


