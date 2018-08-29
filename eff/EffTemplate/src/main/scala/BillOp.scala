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

case class CheckBill(bill: String) extends BillOp[Boolean]

case class UpdateBill(bill: String, status: String)
    extends BillOp[Option[String]]

object BillOp {
  import org.atnos.eff._

  type _billOp[R] = BillOp |= R

  implicit val nt = new (BillOp ~> Eval) {

    def apply[A](fa: BillOp[A]): Eval[A] =
      fa match {
        case UpdateBill(bill, card) => Now(Some("s"))
        case CheckBill(bill)        => Now(true)
      }
  }
}
