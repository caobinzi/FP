import scalaz._
import Scalaz._
object Validate {

  def checkUserDb(
      userName: String
  ): ValidationNel[String, Unit] = {
    val userDb = List("User1", "User2")
    if (!userDb.contains(userName)) {
      return ("User does not exist").failureNel
    } else {
      return ().successNel
    }
  }
  def checkPasswordFromBlackList(
      password: String
  ): ValidationNel[String, Unit] = {
    val passwordBlackList = List("bad1", "bad2")
    if (passwordBlackList.contains(password)) {
      return "password too simple ".failureNel
    } else {
      return ().successNel

    }
  }

  def checkPasswordLength(
      password: String
  ): ValidationNel[String, Unit] = {
    if (password.length < 10) {
      return "Password too short".failureNel
    }
    if (password.length > 50) {
      return "Password too long".failureNel
    }
    return ().successNel
  }
  def checkUsedPassword(
      password: String
  ): ValidationNel[String, Unit] = {

    val usedPassword = List("usedpwd1", "usedpwd2")
    if (usedPassword.contains(password)) {
      "password has been used before".failureNel
    } else {
      ().successNel
    }
  }

  def changePassword(
      userName: String,
      password: String
  ): ValidationNel[String, User] = {

    val checkResult =
      List(
        checkUserDb(userName),
        checkPasswordFromBlackList(password),
        checkPasswordLength(password),
        checkUsedPassword(password)
      ).reduce(_ |+| _)
    checkResult.map(_ => User(userName))
  }
}
