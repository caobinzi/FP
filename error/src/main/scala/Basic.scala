object Basic {
  //Some problems in below code
  //1. Each check may be from differnt systems, the will be code hard to maintain if there are
  //more than 20 checks, disable some of the checks might be hard
  //2. Changing any check will need to modify this big function, can I put it somewhere else
  //3. I also want to get all the errors instead of returning them one by one
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
  def run = {
    changePassword("user", "pwd") match {
      case Left(error) =>
        println(s"Got error: ${error.toList.mkString(",")}")
      case Right(s) =>
        println(s"Got user ${s}")
    }
  }

}
