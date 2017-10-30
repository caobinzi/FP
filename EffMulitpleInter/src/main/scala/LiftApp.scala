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
      cat  <- askUser("What's the kitty's name?")
      _    <- tellUser(s"Current cats: ${cat}")
      _    <- addCat(cat)
      cats <- getAllCats
      _    <- tellUser("Current cats: " + cats.mkString(", "))
    } yield ()
  type Stack = Fx.fx2[Interact, DataOp]
  println("Basic Stack 1 ---->")

  runInteract(runDataOp(program[Stack])).run

  println("Log Stack 2 --->")
  type Stack2 = Fx.fx4[Interact, DataOp, Writer[String, ?], Reader[String, ?]]
  val (r, logs) = (runInteractTranslate(runDataOp(program[Stack2].logTimes[Interact]))
    .runReader("sss")
    .runWriter
    .run)
  logs.foreach(println)
  println(r)

  println("Log Stack 3 ----->")
  type Stack3 = Fx.fx3[Interact, DataOp, Writer[String, ?]]
  val (r3, logs3) =
    (runInteract(runDataOp(program[Stack3].logTimes[DataOp].logTimes[Interact])).runWriter.run)
  logs3.foreach(println)

  println("Log Stack 4 ----->")

  type Stack4 = Fx.fx3[Interact, DataOp, Writer[DataOp[_], ?]]
  val (r4, logs4) =
    (runInteract(runDataOp(program[Stack4].trace[DataOp])).runWriter.run)
  println(logs4)

}
