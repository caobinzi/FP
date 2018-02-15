import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.atnos.eff.all._

import cats.implicits._
import org.atnos.eff._, interpret._

sealed trait IvrOp[A]

sealed trait Result
case object Continue extends Result
case object RequestAgain extends Result
case object Stop extends Result

case class Request(prompt: String) extends IvrOp[String]
case class Response(msg: String) extends IvrOp[Unit]
case class CheckInput(msg: String) extends IvrOp[Result]

object IvrOp {
  import org.atnos.eff._
  type _ivrOp[R] = IvrOp |= R
  type WriterString[A] = Writer[String, A]
  def myDate = new java.util.Date
  def readLine(): String = scala.io.StdIn.readLine()

  def ask(s: String): String = {
    println(s)
    Thread.sleep(2000)
    readLine()
  }
  def tell(msg: String) = println(msg)

  def check(msg: String): Result = msg match {
    case "0" => Stop
    case "1" => RequestAgain
    case _   => Continue
  }

  val nt = new (IvrOp ~> Eval) {
    def apply[A](fa: IvrOp[A]): Eval[A] =

      fa match {
        case Request(prompt) => Now(ask(prompt))
        case Response(msg)   => Now(tell(msg))
        case CheckInput(msg) => Now(check(msg))

      }
  }

  implicit class IVR[R, U, A](effect: Eff[R, A]) {
    def runIvr(implicit sr: Member.Aux[IvrOp, R, U]) =
      transform(effect, nt).runEval
  }

}
