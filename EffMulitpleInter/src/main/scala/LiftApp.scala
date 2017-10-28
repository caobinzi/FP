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
      _    <- tellUser(s"Current cats: ${cat}")
      _    <- addCat(cat)
      cats <- getAllCats
      _    <- tellUser("Current cats: " + cats.mkString(", "))
    } yield ()
  type Stack = Fx.fx2[Interact, DataOp]
  println("Run Stack 1...")

  runInteract(runDataOp(program[Stack])).run

  println("Run Stack 21...")
  type Stack2 = Fx.fx4[Interact, DataOp, Writer[String, ?], Reader[String, ?]]
  val (r, logs) = (runInteractTranslate(runDataOp(program[Stack2])).runReader("sss").runWriter.run)
  logs.foreach(println)
  //type Stack2 = Fx.fx3[Interact, DataOp, WriterString]
  //runInteract(runDataOp(program[Stack2]))

}
