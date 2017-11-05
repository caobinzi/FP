import scalaz._
import Scalaz._
object Test extends App {
  type MyMonad[A] = EitherT[Option, String, A]
  42.point[MyMonad] >>= { x: Int =>
    (x + 1).point[MyMonad]
  }
  // Option[scalaz.\/[String,Int]] = Some(\/-(42))
  // EitherT(Some(\/-(42)))

  val a = for {
    x <- List("1", "2").liftM[OptionT]
    y <- List("3", "4").liftM[OptionT]
  } yield ((x, y))

  //OptionT(List(Some((x,y)), Some((x,y)), Some((x,y)), Some((x,y))))

  type MyMonad2[A] = ListT[MyMonad, A]
  val b = 111.point[MyMonad2]
  //b: MyMonad2[Int] = ListT(EitherT(Some(\/-(List(111)))))

  println(a)
}
