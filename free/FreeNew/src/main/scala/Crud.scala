import scalaz._
import scalaz.Scalaz._
import Helper._
trait GenCrudCompanion {
  type Key
  type Value
  sealed trait Crud[A]
  case class Create(key: Key, value: Value) extends Crud[Boolean]
  case class Read(key: Key)                 extends Crud[Option[Value]]
  case class Update(key: Key, value: Value) extends Crud[Boolean]
  case class Delete(key: Key)               extends Crud[Boolean]
}

object Crud extends GenCrudCompanion {
  type Key = Int
  type Value = String
  type Result[A] = State[Map[Crud.Key, Crud.Value], A]
}

import Crud._
object Crudinterpreter extends (Crud ~> Result) {
  def apply[A](crud: Crud[A]): Result[A] = crud match {
    case Create(key, value) => State { m => (m + (key -> value), m contains key) }
    case Read(key)          => State { m => (m, m get key) }
    case Update(key, value) => State { m => (m + (key -> value), m contains key) }
    case Delete(key)        => State { m => (m - key, m contains key) }
  }
}
object Id2Result extends (Id ~> Result) {
  def apply[A](id: Id[A]): Result[A] = State { m => (m, id) }
}

