import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._
import scala.concurrent._
import ExecutionContext.Implicits.global

object FutureApp extends App {
  val task1 = Future{
    println("task1 start")
    Thread.sleep(10000)
    println("task1 end")
    1
  }

  val task2 = Future{
    println("task2 start")
    Thread.sleep(10000)
    println("task2 end")
    2
  }
  def task(s: Int) = Future{
    println(s"task ${s} start")
    Thread.sleep(10000)
    println(s"task ${s} end")
    s
  }

  //  val task3 = for {
  //    t1 <- task1
  //    t2 <- task2
  //  } yield {
  //    t1 + t2
  //    println("task3 Got here")
  //  }
  val task4 = List(10, 11, 12).map(task).sequenceU.foreach {
    case x =>
      println(x)
      println("task4 Got here")
  }

  Thread.sleep(1000000)

}
