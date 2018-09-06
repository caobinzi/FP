import cats._
import data._
import org.atnos.eff._
import scala.concurrent.{ExecutionContext, Future, Promise}
import org.atnos.eff.addon.monix._
import org.atnos.eff.addon.monix.task._
import org.atnos.eff.syntax.addon.monix.task._
import monix.eval._
import monix.execution._

sealed trait BillOp[A]

case class CheckBill(bill: String) extends BillOp[Boolean]

case class UpdateBill(bill: String, status: String) extends BillOp[Task[String]]

object BillOp {

  type _billOp[R] = BillOp |= R

  def updateBill: String = {
    (1 to 5).foreach { x =>
      Thread.sleep(1000)
      println("updating")
    }
    "Finished"
  }

  val ntTask = new (BillOp ~> Id) {

    def apply[A](fa: BillOp[A]): A =
      fa match {
        case UpdateBill(bill, card) => Task.delay(updateBill)

        case CheckBill(bill) => true
      }
  }
}
