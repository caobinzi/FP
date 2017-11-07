import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import cats.implicits._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import scala.util.Random

object EffOptionApp extends App {

  import org.atnos.eff._
  import KVStore._
  import EffHelper._
  type _option[R] = Option |= R

  def program[R: _kvstore]: Eff[R, Unit] =
    for {
      _ <- Put("wild-cats", 2)
      _ <- Put("tame-cats", 5)
      n <- Get("wild-cats")
      r = n.map(_ * 2)
      _ <- Delete("tame-cats")
    } yield ()

  def done[R: _kvstore]: Eff[R, Boolean] = Check

  type Stack = Fx.fx2[Option, KVStore]
  val pp      = program[Stack].untilM_(done[Stack])
  val result1 = UnSafeIter.runKVStoreUnsafe(pp).runOption.run

  println(result1)

}
