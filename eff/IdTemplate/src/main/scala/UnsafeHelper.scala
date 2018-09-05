import cats._
import data._
import org.atnos.eff._
import cats.implicits._
import org.atnos.eff._, interpret._

object UnsafeHelper {

  def runUnSafe[T[_], R, A, U](
      effects: Eff[R, A],
      haha:    T ~> Id
  )(
      implicit m: Member.Aux[T, R, U]
  ): Eff[U, A] = {

    val sideEffect = new SideEffect[T] {
      def apply[X](fsc: T[X]): X = haha(fsc)

      def applicative[X, Tr[_]: Traverse](ms: Tr[T[X]]): Tr[X] =
        ms.map(apply)
    }
    Interpret.interpretUnsafe(effects)(sideEffect)(m)
  }

  implicit class RunHelper[T[_], R, A, U](effects: Eff[R, A]) {

    def runEffect(haha: T ~> Id)(
        implicit m:     Member.Aux[T, R, U]
    ): Eff[U, A] = runUnSafe(effects, haha)

  }

}
