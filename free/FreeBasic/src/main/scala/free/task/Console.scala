 package free.task
import scalaz._
import scalaz.Scalaz._
import free._

 object Console extends (Interact ~> Id) {
    import Interact._
    def apply[A](i: Interact[A]): Id[A] = i match {
      case Ask(prompt) => { println(prompt); scala.io.StdIn.readLine() }
      case Tell(msg) => println(msg)
    }
  }


