import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

sealed trait KVStore[A]

case class Put(key:    String, value: Int) extends KVStore[Unit]
case class Get(key:    String) extends KVStore[Option[Int]]
case class Delete(key: String) extends KVStore[Unit]

object KVStore {
  import org.atnos.eff._
  type _kvstore[R] = KVStore |= R
}
