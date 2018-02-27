import scalaz._
import Scalaz._
object Validate {

  def checkUserDb(
      userName: String
  ): ValidationNel[String, Unit] = {
    val userDb = List("User1", "User2")
    userDb
      .contains(userName)
      .fold(
        f = ("User does not exist").failureNel,
        t = ().successNel
      )
  }

  def checkPasswordFromBlackList(
      password: String
  ): ValidationNel[String, Unit] = {
    val passwordBlackList = List("bad1", "bad2")
    passwordBlackList
      .contains(password)
      .fold(
        t = "password too simple ".failureNel,
        f = ().successNel
      )
  }

  def checkShortPassword(
      password: String
  ): ValidationNel[String, Unit] = {
    (password.length < 10).fold(
      t = "Password too short".failureNel,
      f = ().successNel
    )
  }

  def checkLongPassword(
      password: String
  ): ValidationNel[String, Unit] = {
    (password.length > 50).fold(
      t = "Password too long".failureNel,
      f = ().successNel
    )
  }

  def checkUsedPassword(
      password: String
  ): ValidationNel[String, Unit] = {

    val usedPassword = List("usedpwd1", "usedpwd2")
    usedPassword
      .contains(password)
      .fold(
        t = "password has been used before".failureNel,
        f = ().successNel
      )
  }

  def changePassword(
      userName: String,
      password: String
  ): ValidationNel[String, User] = {

    val checkResult =
      List(
        checkUserDb(userName),
        checkPasswordFromBlackList(password),
        checkLongPassword(password),
        checkShortPassword(password),
        checkUsedPassword(password)
      ).reduce(_ |+| _)

    //If it is ok, return the userName anyway
    checkResult.map(_ => User(userName))
  }
  def run = {
    changePassword("user", "pwd") match {
      case Success(s) =>
        println(s"Got user ${s}")
      case Failure(error) =>
        println(s"Got error: ${error.toList.mkString(",")}")
    }
  }
}
