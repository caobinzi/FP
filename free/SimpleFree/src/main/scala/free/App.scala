import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._
import concurrent._

object FreeApp extends App {

  import FreeHelper._
  import Store._

  val prg: Free.FreeC[Interact, Boolean] = {
    import LiftImplicit._
    import Interact._
    for {
      key <- Ask("Which key (Integer)?")
      _ <- Tell("finished roundtrip")
    } yield true
  }

  val result: Id[Boolean] = prg.foldMap(Coyoneda.liftTF(Console))
  println(result)

}
