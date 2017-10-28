import cats._
import cats.implicits._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.interpret._
import org.atnos.eff.syntax.all._

object LogHelper {
  type WriterString[A] = Writer[String, A]
  implicit class LogTimesOps[R, A](e:    Eff[R, A]) {
    def logTimes[T[_]](implicit memberT: MemberInOut[T, R],
                       writer:           MemberIn[WriterString, R]): Eff[R, A] =
      logTimes2[R, T, A](e)
  }

  def logTimes2[R, T[_], A](eff:                    Eff[R, A])(implicit memberT: MemberInOut[T, R],
                                            writer: MemberIn[WriterString, R]): Eff[R, A] = {
    translateInto(eff)(new Translate[T, R] {
      def apply[X](tx: T[X]): Eff[R, X] =
        tell[R, String](s"${new java.util.Date}:$tx start") >>
          send[T, R, X](tx) <<
          tell[R, String](s"${new java.util.Date}:$tx end")
    })
  }

}
