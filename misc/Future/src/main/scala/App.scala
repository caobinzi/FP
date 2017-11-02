import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._
import scala.concurrent._
import ExecutionContext.Implicits.global

object FutureApp extends App {
  def task(s: Int) = Future{
    println(s"task ${s} start")
    Thread.sleep(10000)
    println(s"task ${s} end")
    s
  }

  def task1 = Future{
    println(s"task 1 start")
    Thread.sleep(10000)
    println(s"task 1 end")
    1
  }
  def task2 = Future{
    println(s"task 2 start")
    Thread.sleep(10000)
    println(s"task 2 end")
    2
  }



   // val task3 = for {
   //   t1 <- task1
   //   t2 <- task2
   // } yield {
   //   t1 + t2
   //   println("task3 Got here")
   // }

  //val task4 = List(10, 11, 12).map(task).sequenceU.foreach {
   // case x =>
    //  println(x)
     // println("task4 Got here")
  //}

   val task5 = for {
     t1 <- Future{println("11111"); Thread.sleep(10000); 1}
     t2 <- Future{println("22222"); Thread.sleep(10000); 100}
   } yield {
     t1 + t2
     println("task3 Got here")
   }




  Thread.sleep(1000000)

}
