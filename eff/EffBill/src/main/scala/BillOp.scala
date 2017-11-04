import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.implicits._
import org.atnos.eff._, interpret._

sealed trait BillOp[A]

case class PaymentAdvice(bill: String, card: String) extends BillOp[Option[String]]
case class GetAllCats() extends BillOp[List[String]]

object BillOp {
  import org.atnos.eff._
  type _dataOp[R] = BillOp |= R
  def runBillOp[R, A](effect: Eff[R, A])(implicit m: BillOp <= R): Eff[m.Out, A] = {
    val memDataSet = new scala.collection.mutable.ListBuffer[String]

    recurse(effect)(new Recurser[BillOp, m.Out, A, A] {
      def onPure(a: A): A = a

      def onEffect[X](i: BillOp[X]): X Either Eff[m.Out, A] = Left {
        i match {
          case PaymentAdvice(bill, card) => "Ok".some
          case GetAllCats()              => memDataSet.toList
        }
      }

      def onApplicative[X, T[_]: Traverse](ms: T[BillOp[X]]): T[X] Either BillOp[T[X]] =
        Left(ms.map {
          case PaymentAdvice(bill, card) => "OK".some
          case GetAllCats()              => memDataSet.toList
        })
    })(m)
  }
  implicit class Bill[R, A](effect: Eff[R, A]) {
    def runBill(implicit m:         BillOp <= R): Eff[m.Out, A] =
      runBillOp[R, A](effect)

  }
}
