 package free.reader
import scalaz._
import scalaz.Scalaz._
import free._
import FreeHelper.FreeReader
object Printer extends (Log ~> FreeReader) {
    import Log._
    def apply[A](l: Log[A]): FreeReader[A] = l match {
      case Info(txt) => Reader { Unit => println(txt) }
      case Warn(txt) => Reader { Unit => System.err.println(txt)}
    }
  }


