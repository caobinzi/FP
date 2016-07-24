import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import scalaz.Scalaz._


object LogApp extends App {
  println(WriterLog.log)
  //WriterTLog.log
  //OptionTWriterLog.log
  //OptionWriterLog.log
}

