import cats._
import data._
import org.atnos.eff._

sealed trait BillOp[A]

case class CheckBill(bill: String) extends BillOp[Boolean]

case class UpdateBill(bill: String, status: String)
    extends BillOp[Option[String]]

object BillOp {

  type _billOp[R] = BillOp |= R

  val nt = new (BillOp ~> Eval) {

    def apply[A](fa: BillOp[A]): Eval[A] =
      fa match {
        case UpdateBill(bill, card) => Now(Some("s"))
        case CheckBill(bill)        => Now(true)
      }
  }
}
