package free
sealed trait Log[A]
object Log {
  case class Info(txt: String) extends Log[Unit]
  case class Warn(txt: String) extends Log[Unit]
}

sealed trait PPLog[A]
object PPLog {
  case class PPInfo(txt: String) extends PPLog[Unit]
  case class PPWarn(txt: String) extends PPLog[Unit]
}


