import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.implicits._
import org.atnos.eff._, interpret._

sealed trait BankOp[A]

case class Purchase(bill: String, card: String) extends BankOp[Option[String]]
case class Refund(bill: String, card: String) extends BankOp[Option[String]]

object BankOp {
  import org.atnos.eff._

  type _bankOp[R] = BankOp |= R
  def purchase(bill: String, card: String) = "Ok".some
  def check(bill: String) = bill === "1234"
  val nt = new (BankOp ~> Eval) {
    def apply[A](fa: BankOp[A]): Eval[A] =
      fa match {
        case Purchase(bill, card) => Now(purchase(bill, card))
        case Refund(bill, card)   => Now("Ok".some)
      }
  }

  implicit class Bank[R, U, A](effect: Eff[R, A]) {
    def runBank(implicit sr: Member.Aux[BankOp, R, U]) =
      transform(effect, nt).runEval

  }

}
