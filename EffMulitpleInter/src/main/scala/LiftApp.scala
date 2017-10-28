import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import cats.implicits._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object EffOptionApp extends App {

  type WriterString[A] = Writer[String, A]
  import org.atnos.eff._
  import Interact._
  import DataOp._
  import EffHelper._
  import org.atnos.eff._

  def program[R: _interact: _dataOp]: Eff[R, Unit] =
    for {
      cat  <- askUser("What's the kitty's name?")
      _    <- tellUser("Current cats: --")
      _    <- addCat(cat)
      cats <- getAllCats
      _    <- tellUser("Current cats: " + cats.mkString(", "))
    } yield ()
  type Stack = Fx.fx2[Interact, DataOp]

  runInteract(runDataOp(program[Stack])).run
  //type Stack2 = Fx.fx3[Interact, DataOp, WriterString]
  //runInteract(runDataOp(program[Stack2]))

}
