import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._

object FreeApp extends App {
  import Helper._

  type Result[A] = State[Map[Int, String], A]

  def prg[F[_]](implicit I: Interact -~> F, C: Crud -~> F, L: Log -~> F, P:PPLog -~> F): Free[F, Boolean] = {
    import Interact._
    import Crud._
    import Log._
    import PPLog._
    def askFor[T](question: String)(extract: String => T): Free[F, T] = {
      for {
        str <- Ask(question)
        t <- Try(extract(str)).toOption.fold(askFor(question)(extract))(Free.point)
      } yield t
    }
    for {
      key <- askFor("Which key (Integer)?")(_.toInt)
      c <- Create(key, "A")
      _ <- Info(s"created key $key which did${if (c) "" else "n't"} exist")
      vOpt <- Read(key)
      _ <- Info(s"read key $key : $vOpt")
      _ <- PPInfo(s"PP here")
      d1 <- Delete(key)
      _ <- Info(s"deleted key $key which did${if (d1) "" else "n't"} exist")
      d2 <- Delete(key)
      _ <- Info(s"deleted key $key which did${if (d2) "" else "n't"} exist")
      _ <- Create(key, "A")
      _ <- Tell("finished roundtrip")
      passed = !c && vOpt.isDefined && d1 && !d2
    } yield passed: Boolean
  }

  type PRG0[A] = Coproduct[Interact, Crud, A]
  type PRG[A] = Coproduct[Log, PRG0, A]
  type PRG1[A] = Coproduct[PPLog, PRG, A]
  val program: Free[PRG1, Boolean] = prg[PRG1]
  val interpreter0: PRG0 ~> Result = (Console andThen Id2Result) or Crudinterpreter
  val interpreter: PRG ~> Result = (Printer andThen Id2Result) or interpreter0
  val interpreter1: PRG1 ~> Result = (PPPrinter andThen Id2Result) or interpreter
  val result0: Result[Boolean] = program.foldMap(interpreter1)
  val result: (Map[Int, String], Boolean) = result0.run(initial = Map.empty)
  println(result)

}
