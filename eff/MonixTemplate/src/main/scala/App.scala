import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.addon.cats.effect.IOEffect._
import scala.concurrent.{ExecutionContext, Future, Promise}
import org.atnos.eff.syntax.addon.cats.effect._
import cats.effect.IO
import scala.concurrent._
import duration._
import org.atnos.eff.addon.monix._
import org.atnos.eff.addon.monix.task._
import org.atnos.eff.syntax.addon.monix.task._
import monix.eval._
import monix.execution._

import scala.concurrent.duration._

object EffApp extends App {

  import IvrOp._
  import BillOp._
  import BankOp._
  import EffHelper._
  import IOHelper._
  import LogOp._
  import LogHelper._

  def program[R: _ivrOp: _billOp: _bankOp: _logOp: _task]: Eff[R, Unit] = {
    for {
      bill        <- Request("Please type in your bill reference ")
      _           <- Info("====haha===")
      _           <- Response(s"Your bill reference: ${bill}")
      card        <- Request("Please type in your credit card info ")
      _           <- Response(s"Your credit card is : ${card}, we are processing now")
      reference   <- Purchase(bill, card)
      receiptTask <- UpdateBill(bill, "Paid")
      receipt     <- fromTask(receiptTask)
      _           <- Response(s"Refrence ${receipt}")
    } yield ()
  }

  type Stack = Fx.fx5[IvrOp, BillOp, BankOp, LogOp, Task]

  implicit val scheduler = monix.execution.Scheduler(
    java.util.concurrent.Executors.newScheduledThreadPool(10))

  val r = program[Stack]
    .logTimes[IvrOp]
    .logTimes[BillOp]
    .runEffect(IvrIter.nt)
    .runEffect(BankOp.nt)
    .runEffect(LogOp.nt)
    .runEffect(BillOp.ntTask)
    .runAsync // Monix Task
    .runAsync
  println("Getting here")

  //   .runFuture

}
