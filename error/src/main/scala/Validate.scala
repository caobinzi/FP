import scalaz._
import Scalaz._
object Validate {

  def checkUserDb(
      userName: String
  ): ValidationNel[String, Int] = {
    val userDb = List("User1", "User2")
    userDb
      .contains(userName)
      .fold(
        f = ("User does not exist").failureNel,
        t = 1.successNel
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

  def sendUserEmail(
      id: Int
  ): ValidationNel[String, String] = {

    (id === 1)
      .fold(
        f = "Invalid user Id".failureNel,
        t = "jack@email.com".successNel
      )
  }

}
