import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.atnos.eff.all._

import cats.implicits._
import org.atnos.eff._, interpret._
sealed trait Interact[A]

case class Ask(prompt: String) extends Interact[String]
case class Tell(msg:   String) extends Interact[Unit]
case object Stop extends Interact[Unit]

object Interact {
  import org.atnos.eff._
  type _interact[R]    = Interact |= R
  type WriterString[A] = Writer[String, A]
  def myDate = new java.util.Date
  def readLine(): String = scala.io.StdIn.readLine()

  def runInteract[R, A](effect: Eff[R, A])(implicit m: Interact <= R): Eff[m.Out, A] =
    recurse(effect)(new Recurser[Interact, m.Out, A, A] {
      def onPure(a: A): A = a
      def ask(s:    String): String = {
        println(s)
        Thread.sleep(2000)
        readLine()
      }
      def tell(msg: String) = println(msg)

      def onEffect[X](i: Interact[X]): X Either Eff[m.Out, A] =
        i match {
          case Ask(prompt) => Left(ask(prompt))
          case Tell(msg)   => Left(tell(msg))
        }

      def onApplicative[X, T[_]: Traverse](ms: T[Interact[X]]): T[X] Either Interact[T[X]] =
        Left(ms.map {
          case Ask(prompt) => ask(prompt)
          case Tell(msg)   => tell(msg)
        })

    })(m)

}
