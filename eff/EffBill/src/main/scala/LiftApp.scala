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
  import Interact._
  import DataOp._
  import EffHelper._
  import LogHelper._

  def program[R: _interact: _dataOp]: Eff[R, Unit] =
    for {
      cat  <- Ask("What's the kitty's name?")
      cats <- Tell(s"Current cats: ${cat}") >> AddCat(cat) >> GetAllCats()
      _    <- Tell("Current cats: " + cats.mkString(", "))
    } yield ()
  type Stack6 = Fx.fx3[Interact, DataOp, Writer[String, ?]]
  val (r6, logs6) =
    (runInteract(runDataOp(program[Stack6].logTimes[Interact])).runWriter.run)
  logs6.foreach(println)
}
