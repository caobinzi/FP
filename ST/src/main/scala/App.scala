import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import scalaz.Scalaz._
import scalaz.effect._

import ST._

case class Foo(a:Int)
object STApp extends App {

  type ForallST[A] = Forall[ST[?, A]]

  def e1[S] = for {
    x <- newVar[S](Foo(2))
    r <- x mod { x => x.copy(a=x.a+1) }
  } yield x

  def e2[S]: ST[S, Foo] = for {
    x <- e1[S]
    r <- x.read
  } yield r
  println(runST(new ForallST[Foo] { def apply[S] = e2[S] }))
}

