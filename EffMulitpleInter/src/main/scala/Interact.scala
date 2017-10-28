import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import Types._

import cats.implicits._
import org.atnos.eff._, interpret._
sealed trait Interact[A]

case class Ask(prompt: String) extends Interact[String]
case class Tell(msg:   String) extends Interact[Unit]

object Interact {
  import org.atnos.eff._
  type _interact[R]    = Interact |= R
  type WriterString[A] = Writer[String, A]
  def readLine(): String =
    "snuggles"
  def askUser[R: _interact](prompt: String): Eff[R, String] =
    send(Ask(prompt))

  def tellUser[R: _interact](message: String): Eff[R, Unit] =
    send(Tell(message))

  def runInteract[R, A](effect: Eff[R, A])(implicit m: Interact <= R): Eff[m.Out, A] =
    recurse(effect)(new Recurser[Interact, m.Out, A, A] {
      def onPure(a: A): A = a

      def onEffect[X](i: Interact[X]): X Either Eff[m.Out, A] = Left {
        i match {
          case Ask(prompt) =>
            println(prompt)
            readLine()

          case Tell(msg) =>
            println(msg)
        }
      }
      def onApplicative[X, T[_]: Traverse](ms: T[Interact[X]]): T[X] Either Interact[T[X]] =
        Left(ms.map {
          case Ask(prompt) => println(prompt); readLine()
          case Tell(msg)   => println(msg)
        })

    })(m)
  /*
  type _writerString[R] = Writer[String, ?] |= R

  def runLoggerInteract[R, U, A](
      effects:  Eff[R, A]
  )(implicit r: Member.Aux[Interact, R, U], writer: _writerString[U]): Eff[U, A] = {
    type S = Fx.prepend[WriterString, R]
    runInteract(effects)
  }
 */

}
