import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._
import scala.concurrent._
import ExecutionContext.Implicits.global
import com.codahale.metrics._;
import java.util.concurrent.TimeUnit

object YourApplication {
  /** The application wide metrics registry. */
  val metricRegistry = new com.codahale.metrics.MetricRegistry()
}
trait Instrumented extends nl.grons.metrics.scala.InstrumentedBuilder {
  val metricRegistry = YourApplication.metricRegistry
}
object FutureApp extends App with Instrumented {
  val loading = metrics.timer("loading")
  val reporter = ConsoleReporter.forRegistry(metricRegistry)
  .convertRatesTo(TimeUnit.SECONDS)
  .convertDurationsTo(TimeUnit.MILLISECONDS)
  .build();
  reporter.start(1, TimeUnit.SECONDS);

  def task(s: Int) = 
  loading.time{
    println(s"task ${s} start")
    Thread.sleep(1000*s)
    println(s"task ${s} end")
    s
  }
  ( 1 to 1000).foreach(task)

  Thread.sleep(1000000)

}
