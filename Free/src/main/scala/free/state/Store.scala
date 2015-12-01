package free.state
import scalaz._
import scalaz.Scalaz._
import free._
object Store {
  type Store[A] = State[Map[Int, String], A]
  object Crudinterpreter extends (Crud ~> Store) {
    import Crud._
    def apply[A](crud: Crud[A]): Store[A] = crud match {
      case Create(key, value) => State { m => (m + (key -> value), m contains key) }
      case Read(key) => State { m => (m, m get key) }
      case Update(key, value) => State { m => (m + (key -> value), m contains key) }
      case Delete(key) => State { m => (m - key, m contains key) }
    }
  }

  object Id2Store extends (Id ~> Store) {
    def apply[A](id: Id[A]): Store[A] = State { m => (m, id) }
  }

}
