sealed trait Interact[A]
object Interact {
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]
}


