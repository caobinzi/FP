import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.atnos.eff.all._

import cats.implicits._
import org.atnos.eff._, interpret._

sealed trait InteractOp[A]

sealed trait Result
case object Continue extends Result
case object AskAgain extends Result
case object Stop extends Result

case class Ask(prompt: String) extends InteractOp[String]
case class Tell(msg:   String) extends InteractOp[Unit]
case class Check(msg:  String) extends InteractOp[Result]

object InteractOp {
  import org.atnos.eff._
  type _interact[R]    = InteractOp |= R
  type WriterString[A] = Writer[String, A]
  def myDate = new java.util.Date
  def readLine(): String = scala.io.StdIn.readLine()

  def runInteractOp[R, A](effect: Eff[R, A])(implicit m: InteractOp <= R): Eff[m.Out, A] =
    recurse(effect)(new Recurser[InteractOp, m.Out, A, A] {
      def onPure(a: A): A = a
      def ask(s:    String): String = {
        println(s)
        Thread.sleep(2000)
        readLine()
      }
      def tell(msg: String) = println(msg)

      def check(msg: String): Result = msg match {
        case "0" => Stop
        case "1" => AskAgain
        case _   => Continue
      }

      def onEffect[X](i: InteractOp[X]): X Either Eff[m.Out, A] =
        i match {
          case Ask(prompt) => Left(ask(prompt))
          case Tell(msg)   => Left(tell(msg))
          case Check(msg)  => Left(check(msg))
        }

      def onApplicative[X, T[_]: Traverse](ms: T[InteractOp[X]]): T[X] Either InteractOp[T[X]] =
        Left(ms.map {
          case Ask(prompt) => ask(prompt)
          case Tell(msg)   => tell(msg)
          case Check(msg)  => check(msg)
        })

    })(m)

  implicit class Interact[R, A](effect: Eff[R, A]) {
    def runInteract(implicit m:         InteractOp <= R): Eff[m.Out, A] =
      runInteractOp[R, A](effect)
  }

}
