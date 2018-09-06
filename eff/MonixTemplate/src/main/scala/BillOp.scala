import cats._
import data._
import cats.implicits._
import org.atnos.eff._
import scala.concurrent.{ExecutionContext, Future, Promise}
import org.atnos.eff.addon.monix._
import org.atnos.eff.addon.monix.task._
import org.atnos.eff.syntax.addon.monix.task._
import monix.eval._
import monix.execution._

sealed trait BillOp[A]

case class CheckBill(bill: String) extends BillOp[Task[Boolean]]

case class UpdateBill(bill: String, status: String) extends BillOp[Task[String]]

object BillOp {

  type _billOp[R] = BillOp |= R

  def updateBill: String = {
    (1 to 5).foreach { x =>
      Thread.sleep(1000)
      val threadId = Thread.currentThread().getId();
      println(
        s"Updating bills -> waiting 1 seconds and with thread id ${threadId}")
    }
    println("Update Finished")
    "done"
  }

  def checkBill: Boolean = {
    (1 to 5).foreach { x =>
      Thread.sleep(1000)
      val threadId = Thread.currentThread().getId();
      println(
        s"Checking bills -> waiting 1 seconds and with thread id ${threadId}")
    }
    println("Checking Finished")
    true
  }

  val ntTask = new (BillOp ~> Id) {

    def apply[A](fa: BillOp[A]): A =
      fa match {
        case UpdateBill(bill, card) =>
          (Task(checkBill), Task(updateBill)).parMapN(
            (x, y) => "gotcha"
          )
        case CheckBill(bill) => Task(checkBill)
      }
  }
}
