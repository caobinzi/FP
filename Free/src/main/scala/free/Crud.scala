package free
// Moved this out side of the trait. This caused major headaches for me
sealed trait Crud[A]
// A language for creating, reading, updating and deleting which is generic in the key and value type
object Crud {
  case class Create(key: Int, value: String) extends Crud[Boolean]
  case class Read(key: Int) extends Crud[Option[String]]
  case class Update(key: Int, value: String) extends Crud[Boolean]
  case class Delete(key: Int) extends Crud[Boolean]
}

