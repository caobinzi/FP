package free.task
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._
import concurrent._
import free._

object FreeApp extends App {

  import FreeHelper._
  import Store._

  def prg[F[_]](implicit I: Interact -~> F, C: Crud -~> F, L: Log -~> F, P: PPLog -~> F): Free.FreeC[F, Boolean] = {
    import LiftImplicit._
    import Interact._
    import Log._
    import PPLog._
    import Crud._

    def value[T](a: T) = Free.pure[Coyoneda[F, ?], T](a)

    def askFor[T](question: String)(extract: String => T): Free.FreeC[F, T] = {
      for {
        str <- Ask(question)
        t <- Try(extract(str)).toOption.fold(askFor(question)(extract))(value(_))
      } yield t
    }

    for {
      key <- askFor("Which key (Integer)?")(_.toInt)
      _key <- value { if (key > 20) key.some else none }
      _ <- PPInfo(s"Number --> ${_key}")
      c <- _key.fold(value(false))(Create(_, "A"))
      _ <- Info(s"created key $key which did${if (c) "" else "n't"} exist")
      vOpt <- Read(key)
      _ <- Info(s"read key $key : $vOpt")
      d1 <- Delete(key)
      _ <- Info(s"deleted key $key which did${if (d1) "" else "n't"} exist")
      d2 <- Delete(key)
      _ <- Info(s"deleted key $key which did${if (d2) "" else "n't"} exist")
      _ <- Create(key, "A") // just to have no empty map in the end
      _ <- Tell("finished roundtrip")
      passed = !c && vOpt.isDefined && d1 && !d2
    } yield passed: Boolean
  }

  type PRG0[A] = Coproduct[Interact, Crud, A]
  type PRG1[A] = Coproduct[Log, PRG0, A]
  type PRG2[A] = Coproduct[PPLog, PRG1, A]

  def interpreter = {
    val interpreter0: PRG0 ~> Store = (Id2Store compose Console) or Crudinterpreter
    val interpreter1: PRG1 ~> Store = (Id2Store compose Printer) or interpreter0
    val interpreter2: PRG2 ~> Store = (Id2Store compose PPPrinter) or interpreter1
    Coyoneda.liftTF(interpreter2)
  }

  val program: Free.FreeC[PRG2, Boolean] = prg[PRG2]

  val result0: Store[Boolean] = program.foldMap(interpreter)
  val result: Task[Boolean] = result0.run( Map.empty)
  println(result.run)

}
