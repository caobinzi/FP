object CatsSyntaxApp extends App {
  List("abc", "aa/bc", "abb/c", "abcc/", "/abc", "//abc").foreach { x =>
    println(StringParser.run(x))
  }
  println(CompareString.run("abb/c", "abcc/"))
  println(CompareString.run("abc", "aa/bc"))
  println(CompareString.run("a/aaaaabc", "//abc/"))

}
