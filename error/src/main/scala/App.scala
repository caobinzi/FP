import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import Validate._

case class User(userName: String)
object ErroApp extends App {
  val checkResult =
    List(
      checkUserDb("user"),
      checkPasswordFromBlackList("pwd"),
      checkLongPassword("pwd"),
      checkShortPassword("pwd"),
      checkUsedPassword("pwd")
    ).reduce(_ |+| _)

  checkResult match {
    case Success(s) =>
      println(s"Got user ${s}")
    case Failure(error) =>
      println(s"Got error: ${error.toList.mkString(",")}")

  }
}
