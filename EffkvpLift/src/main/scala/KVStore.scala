import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

sealed trait KVStore[+A]

case class Put[T](key: String, value: T) extends KVStore[Unit]
case class Get[T](key: String) extends KVStore[Option[T]]
case class Delete(key: String) extends KVStore[Unit]

object KVStore {
  import org.atnos.eff._

// T |= R is an alias for MemberIn[T, R]
// stating that effects of type T[_] can be injected in the effect stack R
// It is also equivalent to MemberIn[KVStore, R]
  type _kvstore[R] = KVStore |= R
  implicit def converter[A, R: _kvstore](s: KVStore[A]): Eff[R, A] = {
    Eff.send[KVStore, R, A](s)
  }

  /** update composes get and put, and returns nothing. */
  def update[T, R: _kvstore](key: String, f: T => T): Eff[R, Unit] =
    for {
      ot <- Get[T](key)
      _  <- ot.map(t => Put[T](key, f(t)): Eff[R, Unit]).getOrElse(Eff.pure(()))
    } yield ()
}
