import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object EffApp extends App {

  import org.atnos.eff._
  import KVStore._
  type _option[R] = Option |= R

  def program[R: _kvstore]: Eff[R, Option[Int]] =
    for {
      _ <- store("wild-cats", 2)
      _ <- update[Int, R]("wild-cats", _ + 12)
      _ <- store("tame-cats", 5)
      n <- find[Int, R]("wild-cats").map(_.map(_ * 2))
      _ <- delete("tame-cats")
    } yield n

  val result1 = UnSafeIter.runKVStoreUnsafe(program[Fx.fx1[KVStore]]).run
  println(result1)
  type Stack =
    Fx.fx4[KVStore, Throwable Either ?, State[Map[String, Any], ?], Writer[String, ?]]

  val result2 =
    SafeInter.runKVStore(program[Stack]).runEither.evalState(Map.empty[String, Any]).runWriter.run
  println(result2)

}
