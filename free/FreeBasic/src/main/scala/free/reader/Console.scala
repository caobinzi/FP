 package free.reader
import scalaz._
import scalaz.Scalaz._
import free._
import concurrent._
import FreeHelper.FreeReader

 object Console extends (Interact ~> FreeReader) {
    import Interact._
    def apply[A](i: Interact[A]): FreeReader[A] = i match {
      case Ask(prompt) => Reader{ Unit=>{println(prompt); scala.io.StdIn.readLine() }}
      case Tell(msg) => Reader { Unit => {println(msg)}}
    }
  }


