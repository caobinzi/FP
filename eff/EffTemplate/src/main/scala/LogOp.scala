import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.implicits._
import org.atnos.eff._, interpret._
import IntoPoly._

sealed trait LogOp[A]

case class Info(s: String) extends LogOp[Unit]

object LogOp {
  import org.atnos.eff._

  type _logOp[R] = LogOp |= R

  val nt = new (LogOp ~> Eval) {

    def apply[A](fa: LogOp[A]): Eval[A] =
      fa match {
        case Info(s) => Now(println(s))
      }
  }

  implicit class Log[SR, BR, U, A](effect: Eff[SR, A]) {

    def runLog(
        implicit sr: Member.Aux[LogOp, SR, U],
        br:          Member.Aux[Eval, BR, U]
    ): Eff[BR, A] = {
      transform(effect, nt)

    }

  }
}
