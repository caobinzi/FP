import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.interpret._

object EffHelper {
  implicit def liftEff[A, F[_], R: F |= ?](s: F[A]): Eff[R, A] = {
    Eff.send[F, R, A](s)
  }
  implicit class RunHelper[SR, BR, U, A](effect: Eff[SR, A]) {

    def runEffect[T[_]](implicit sr: Member.Aux[T, SR, U],
                        br:          Member.Aux[Eval, BR, U],
                        nt:          T ~> Eval): Eff[U, A] = {
      transform(effect, nt).runEval
    }
  }
}
