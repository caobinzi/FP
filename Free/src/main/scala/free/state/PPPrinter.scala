 package free.state
import scalaz._
import scalaz.Scalaz._
import free._
  object PPPrinter extends (PPLog ~> Id) {
    import PPLog._
    def apply[A](l: PPLog[A]): Id[A] = l match {
      case PPInfo(txt) => println(txt)
      case PPWarn(txt) => System.err.println(txt)
    }
  }


