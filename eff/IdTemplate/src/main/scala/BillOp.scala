import cats._
import data._
import org.atnos.eff._
import scala.concurrent.{ExecutionContext, Future, Promise}

sealed trait BillOp[A]

case class CheckBill(bill: String) extends BillOp[Boolean]

case class UpdateBill(bill: String, status: String)
    extends BillOp[Future[String]]

object BillOp {

  type _billOp[R] = BillOp |= R

  def updateBill = {
    (1 to 5).foreach { x =>
      Thread.sleep(1000)
      println("updating")
    }
    "Finished"
  }
  import scala.concurrent.ExecutionContext.Implicits.global

  val nt = new (BillOp ~> Id) {

    def apply[A](fa: BillOp[A]): Id[A] =
      fa match {
        case UpdateBill(bill, card) => Future(updateBill)
        case CheckBill(bill)        => true
      }
  }
}
