package free.task
import scalaz._
import scalaz.Scalaz._
import free._
import concurrent._
object Store {

  type Store[A] = Kleisli[ Task, Map[Int, String], A ]
  
  //implicit val functor =  Functor[B] compose Functor[Task]
  object Crudinterpreter extends (Crud ~> Store) {
    import Crud._
    def apply[A](crud: Crud[A]): Store[A] = crud match {
      case Create(key, value) => Kleisli{ m => Task{m + (key -> value); m contains key} }
      case Read(key) => Kleisli{ m => Task{m get key} }
      case Update(key, value) => Kleisli{ m => Task{m + (key -> value); m contains key} }
      case Delete(key) => Kleisli{ m => Task{m - key; m contains key} }
    }
  }

  object Id2Store extends (Id ~> Store) {
    def apply[A](id: Id[A]): Store[A] = Kleisli{ m => Task(id) }
  }

}
