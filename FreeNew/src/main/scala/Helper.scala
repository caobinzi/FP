import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Try
import scalaz._
import scalaz.Scalaz._

object Helper {
  type Copro[F[_], G[_]] = { type f[x] = Coproduct[F, G, x] } 

  implicit class NaturalTransformationOrOps[F[_], H[_]](private val nt: F ~> H) extends AnyVal {
    // given F ~> H and G ~> H we derive Copro[F, G]#f ~> H
    def or[G[_]](f: G ~> H): Copro[F, G]#f ~> H =
      new (Copro[F, G]#f ~> H) {
        def apply[A](c: Coproduct[F, G, A]): H[A] = c.run match {
          case -\/(fa) => nt(fa)
          case \/-(ga) => f(ga)
        }
      }
  }

  type -~>[F[_], G[_]] = Inject[F, G] 

  implicit def lift[F[_], G[_], A](fa: F[A])(implicit I: F -~> G): Free[G, A] = Free liftF I.inj(fa)

}

