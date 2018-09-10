import cats._
import cats.implicits._
import cats.syntax._
import cats.data.State
import scala.annotation.tailrec

case class MagicString(
    lastChar: Char,
    left:     List[Char]
) {

  def next: Option[MagicString] = {
    left match {
      case Nil                     => None
      case '/' :: t                => MagicString(lastChar, t).next
      case h :: t if h == lastChar => MagicString(lastChar, t).next
      case h :: t if h != lastChar => Option(MagicString(h, t))
    }
  }
}

object CompareString {

  def compare(
      first:  MagicString,
      second: MagicString
  ): Boolean = {
    println(s"compare ${first}, ${second}")
    (first.next, second.next) match {
      case (_, _) if first.lastChar != second.lastChar => false
      case (None, None)                                => true
      case (Some(a), None)                             => compare(a, second)
      case (None, Some(b))                             => compare(first, b)
      case (Some(a), Some(b))                          => compare(a, b)
    }
  }

  def run(first: String, second: String) = {
    compare(MagicString('/', first.toList), MagicString('/', second.toList))
  }

}
