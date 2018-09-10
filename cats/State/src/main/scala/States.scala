import cats._
import cats.implicits._
import cats.syntax._
import cats.data.State
case class StringParser(lastChar: Char, output: String)

object StringParser {
  import State._

  def parseChar(c: Char, last: StringParser): StringParser =
    c match {
      case '/'                     => last
      case a if a == last.lastChar => last
      case a if a != last.lastChar => StringParser(a, last.output + a)
    }

  def parseNext(c: Char): State[StringParser, Unit] =
    get[StringParser] >>=
      (x => set(parseChar(c, x)))

  def parseString(input: String): State[StringParser, Unit] =
    input.toList.traverse(parseNext).map(_ => ())

  def run(input: String) = {
    StringParser
      .parseString(input)
      .run(StringParser('/', ""))
      .value
      ._1
      .output
  }
}
