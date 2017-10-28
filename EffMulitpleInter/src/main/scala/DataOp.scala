import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import Types._
import cats.implicits._
import org.atnos.eff._, interpret._

sealed trait DataOp[A]

case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]

object DataOp {
  import org.atnos.eff._
  type _dataOp[R] = DataOp |= R
  def addCat[R: _dataOp](a: String): Eff[R, Unit] =
    send(AddCat(a))

  def getAllCats[R: _dataOp]: Eff[R, List[String]] =
    send(GetAllCats())

  def runDataOp[R, A](effect: Eff[R, A])(implicit m: DataOp <= R): Eff[m.Out, A] = {
    val memDataSet = new scala.collection.mutable.ListBuffer[String]

    recurse(effect)(new Recurser[DataOp, m.Out, A, A] {
      def onPure(a: A): A = a

      def onEffect[X](i: DataOp[X]): X Either Eff[m.Out, A] = Left {
        i match {
          case AddCat(a)    => memDataSet.append(a); ()
          case GetAllCats() => memDataSet.toList
        }
      }

      def onApplicative[X, T[_]: Traverse](ms: T[DataOp[X]]): T[X] Either DataOp[T[X]] =
        Left(ms.map {
          case AddCat(a)    => memDataSet.append(a); ()
          case GetAllCats() => memDataSet.toList
        })
    })(m)

  }
}
