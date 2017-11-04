import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import cats.implicits._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.interpret._
import org.atnos.eff.syntax.all._

object EffOptionApp extends App {

  import org.atnos.eff._
  import InteractOp._
  import BillOp._
  import EffHelper._
  import LogHelper._

  def program[R: _interact: _dataOp]: Eff[R, Unit] =
    for {
      bill    <- Ask("Please type in your bill reference ")
      cats    <- Tell(s"Your bill reference: ${bill}")
      card    <- Ask("Please type in your credit card info ")
      cats    <- Tell(s"Your credit card is : ${card}, we are processing now")
      receipt <- PaymentAdvice(bill, card)
      _       <- Tell(s"Your payment refrence is ${receipt}")
    } yield ()
  type Stack6 = Fx.fx3[InteractOp, BillOp, Writer[String, ?]]
  val (r6, logs6) =
    program[Stack6].logTimes[InteractOp].runBill.runInteract.runWriter.run
  logs6.foreach(println)
}
