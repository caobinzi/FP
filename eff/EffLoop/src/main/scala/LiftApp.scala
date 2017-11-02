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

  def program[R: _kvstore: _option]: Eff[R, Boolean] =
    for {
      _        <- fromOption(2.some)
      continue <- fromOption(true.some)
      _        <- Put("wild-cats", 2)
      _        <- Put("tame-cats", 5)
      n        <- Get("wild-cats")
      r = n.map(_ * 2)
      _ <- Delete("tame-cats")
    } yield continue

  def done[R: _kvstore]: Eff[R, Boolean] =
    for {
      continue <- Check
    } yield { println(s"-------Continue is ${continue}"); continue }

  type TT = Fx.fx2[Option, KVStore]
  val pp      = program[TT].untilM_(done[TT])
  val result1 = UnSafeIter.runKVStoreUnsafe(pp).runOption.run

  println(result1)

}
