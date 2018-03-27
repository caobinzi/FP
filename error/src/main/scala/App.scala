import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import Validate._
import Validation.FlatMap._

case class User(userName: String)
object ErroApp extends App {
  val prg =
    for {
      id    <- checkUserDb("User1")
      _     <- checkPasswordFromBlackList("a good password")
      _     <- checkLongPassword("a good password")
      _     <- checkShortPassword("a good password")
      _     <- checkUsedPassword("a good password")
      email <- sendUserEmail(id)
    } yield s"An Email has been sent to ${email}"

  prg match {
    case Success(s) =>
      println(s"All good, Result:  ${s}")
    case Failure(error) =>
      println(s"Got error: ${error.toList.mkString(",")}")

  }

  //Check Only

  val checkOnly = List(
    checkUserDb("User1aa").map(_ => ()),
    checkPasswordFromBlackList("bad1jj"),
    checkLongPassword("a good password"),
    checkShortPassword("short"),
    checkUsedPassword("usedpwd1")
  ).reduce(_ |+| _)

  checkOnly match {
    case Success(s) =>
      println(s"Check passed")
    case Failure(error) =>
      println(s"Check failed: ${error.toList.mkString(", ")}")

  }

}
