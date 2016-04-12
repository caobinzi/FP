import scalaz._
import scalaz.Scalaz._
import Helper._
sealed trait Crud[A]
object Crud {
  case class Create(key: Int, value: String) extends Crud[Boolean]
  case class Read(key: Int) extends Crud[Option[String]]
  case class Update(key: Int, value: String) extends Crud[Boolean]
  case class Delete(key: Int) extends Crud[Boolean]
  type Result[A] = State[Map[Int, String], A]
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

