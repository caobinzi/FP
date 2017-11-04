import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import cats.implicits._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.interpret._
import org.atnos.eff.syntax.all._

object EffBasicApp extends App {

  import org.atnos.eff._
  import IvrOp._
  import BillOp._
  import EffHelper._
  import LogHelper._

  def program[R: _ivr: _dataOp]: Eff[R, Unit] =
    for {
      bill    <- Request("Please type in your bill reference ")
      cats    <- Response(s"Your bill reference: ${bill}")
      card    <- Request("Please type in your credit card info ")
      cats    <- Response(s"Your credit card is : ${card}, we are processing now")
      receipt <- PayBill(bill, card)
      _       <- Response(s"Your payment refrence is ${receipt}")
    } yield ()

  type Stack = Fx.fx2[IvrOp, BillOp]
  program[Stack].runBill.runIvr.run
}
