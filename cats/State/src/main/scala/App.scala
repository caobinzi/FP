object CatsSyntaxApp extends App {
  List("abc", "aa/bc", "abb/c", "abcc/", "/abc", "//abc").foreach { x =>
    println(StringParser.run(x))
  }

}
