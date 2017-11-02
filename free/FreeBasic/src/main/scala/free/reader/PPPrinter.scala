 package free.reader
import scalaz._
import scalaz.Scalaz._
import free._
import FreeHelper.FreeReader
  object PPPrinter extends (PPLog ~> FreeReader) {
    import PPLog._
    def apply[A](l: PPLog[A]): FreeReader[ A] = l match {
      case PPInfo(txt) => Reader{ Unit => println(txt) }
      case PPWarn(txt) => Reader { Unit => System.err.println(txt) }
    }
  }


