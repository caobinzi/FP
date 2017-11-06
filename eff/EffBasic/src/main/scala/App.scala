import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

// useful type aliases showing that the ReaderInt and the WriterString effects are "members" of R
// note that R could have more effects
object EffApp extends App {

  type ReaderInt[A]    = Reader[Int, A]
  type WriterString[A] = Writer[String, A]

  type Stack = Fx.fx3[WriterString, ReaderInt, Eval]

  type _readerInt[R]    = ReaderInt |= R
  type _writerString[R] = WriterString |= R

  def program[R: _readerInt: _writerString: _eval]: Eff[R, Int] =
    for {
      n <- ask[R, Int]
      _ <- tell("the required power is " + n)
      a <- delay(math.pow(2, n.toDouble).toInt)
      _ <- tell("the result is " + a)
    } yield a

  println(program[Stack].runReader(6).runWriter.runEval.run)
  //(64,List(the required power is 6, the result is 64))
  println(program[Stack].runWriter.runReader(6).runEval.run)
  //(64,List(the required power is 6, the result is 64))

}
