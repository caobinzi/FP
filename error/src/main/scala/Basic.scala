object Basic {
  def changePassword(
      userName: String,
      password: String
  ): Either[String, User] = {
    val userDb = List("User1", "User2")
    val passwordBlackList = List("bad1", "bad2")
    val usedPassword = List("usedpwd1", "usedpwd2")

    if (!userDb.contains(userName)) {
      return Left("User does not exist")
    }
    if (password.length < 10) {
      return Left("Password too short")
    }
    if (password.length > 50) {
      return Left("Password too long")
    }
    if (passwordBlackList.contains(password)) {
      return Left("password too simple ")
    }
    if (usedPassword.contains(password)) {
      return Left("password has been used before")
    }
    return Right(User(userName))
  }
}
