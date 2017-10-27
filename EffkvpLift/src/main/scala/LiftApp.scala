import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import cats.implicits._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object EffOptionApp extends App {

  import org.atnos.eff._
  import KVStore._
  type _option[R] = Option |= R

  def program[R: _kvstore: _option]: Eff[R, Option[Int]] =
    for {
      _ <- fromOption(2.some)
      _ <- Put("wild-cats", 2)
      _ <- update[Int, R]("wild-cats", _ + 12)
      _ <- Put("tame-cats", 5)
      n <- Get[Int]("wild-cats").map(_.map(_ * 2))
      _ <- Delete("tame-cats")
    } yield n

  val result1 = UnSafeIter.runKVStoreUnsafe(program[Fx.fx2[KVStore, Option]]).runOption.run
  println(result1)
  type Stack =
    Fx.fx5[KVStore, Option, Throwable Either ?, State[Map[String, Any], ?], Writer[String, ?]]

  val result2 =
    SafeInter
      .runKVStore(program[Stack])
      .runEither
      .evalState(Map.empty[String, Any])
      .runWriter
      .runOption
      .run
  println(result2)

}
