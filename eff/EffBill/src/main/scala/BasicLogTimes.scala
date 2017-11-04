import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import cats.implicits._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.interpret._
import org.atnos.eff.syntax.all._

object EffBasicLogTimesApp extends App {

  import org.atnos.eff._
  import InteractOp._
  import BillOp._
  import EffHelper._
  import LogHelper._

  def checkInput[R: _interact](input: String): Eff[R, Option[String]] =
    (Check(input): Eff[R, Result]) >>= { r =>
      r match {
        case Continue => Eff.pure(input.some)
        case Stop     => Eff.pure(None)
        case AskAgain => askBill[R]
      }
    }

  def askBill[R: _interact]: Eff[R, Option[String]] =
    for {
      input <- Ask("Please type in your bill reference or type 0 to stop")
      bill  <- checkInput(input)
    } yield bill

  def program[R: _interact: _dataOp: _option]: Eff[R, Unit] =
    for {
      billOption <- askBill
      bill       <- fromOption(billOption)
      cats       <- Tell(s"Your bill reference: ${bill}")
      card       <- Ask("Please type in your credit card info ")
      cats       <- Tell(s"Your credit card is : ${card}, we are processing now")
      receipt    <- PayBill(bill, card)
      _          <- Tell(s"Your payment refrence is ${receipt}")
    } yield ()

  type Stack = Fx.fx4[InteractOp, BillOp, Writer[String, ?], Option]

  val (result, logs) =
    program[Stack].logTimes[BillOp].runBill.runInteract.runOption.runWriter.run
  logs.foreach(println)
}
