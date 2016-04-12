import scalaz._
import scalaz.Scalaz._
import Helper._
sealed trait Interact[A]
object Interact {
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]
}
object Console extends (Interact ~> Id) {
  import Interact._
  def apply[A](i: Interact[A]): Id[A] = i match {
    case Ask(prompt) => { println(prompt); scala.io.StdIn.readLine() }
    case Tell(msg)   => println(msg)
  }
}

