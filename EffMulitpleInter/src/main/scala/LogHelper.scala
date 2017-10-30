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
        for {
          _ <- tell[R, String](s"${new java.util.Date}:$tx start")
          x <- send[T, R, X](tx)
          _ <- tell[R, String](s"${new java.util.Date}:$tx end")
        } yield x
    })
  }
  def augmentBA[R, T[_], O[_], A](eff: Eff[R, A])(beforeW: Augment[T, O], afterW: Augment[T, O])(
      implicit memberT:                MemberInOut[T, R],
      memberO:                         MemberIn[O, R]): Eff[R, A] = {
    translateInto(eff)(new Translate[T, R] {
      def apply[X](tx: T[X]): Eff[R, X] =
        for {
          _ <- send[O, R, Unit](beforeW(tx))
          x <- send[T, R, X](tx)
          _ <- send[O, R, Unit](afterW(tx))
        } yield x
    })
  }
  def writeBA[R, T[_], O, A](eff: Eff[R, A])(bW: Write[T, O], aW: Write[T, O])(
      implicit memberT:           MemberInOut[T, R],
      memberW:                    MemberIn[Writer[O, ?], R]): Eff[R, A] = {
    augmentBA[R, T, Writer[O, ?], A](eff)(
      beforeW = new Augment[T, Writer[O, ?]] {
        def apply[X](tx: T[X]) = Writer.tell[O](bW(tx))
      },
      afterW = new Augment[T, Writer[O, ?]] {
        def apply[X](tx: T[X]) = Writer.tell[O](aW(tx))
      }
    )
  }
  def traceBA[R, T[_], A](eff:                     Eff[R, A])(implicit memberT: MemberInOut[T, R],
                                          memberW: MemberInOut[Writer[String, ?], R]): Eff[R, A] =
    writeBA[R, T, String, A](eff)(
      bW = new Write[T, String] {
        def apply[X](tx: T[X]): String = s"${new java.util.Date} ${tx} Start"
      },
      aW = new Write[T, String] {
        def apply[X](tx: T[X]): String = s"${new java.util.Date} ${tx} End"
      }
    )

  implicit class EffTranslateIntoOps[R, A](val e: Eff[R, A]) extends AnyVal {

    def traceAfter[F[_]](implicit memberF: MemberInOut[F, R],
                         memberW:          MemberInOut[Writer[String, ?], R]): Eff[R, A] =
      LogHelper.traceBA[R, F, A](e)

  }
}
