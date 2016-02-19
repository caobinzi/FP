import scalaz._
import scalaz.Scalaz._
object FreeHelper {
  implicit class NaturalTransformationOrOps[F[_], H[_]](private val nt: F ~> H) extends AnyVal {
    def or[G[_]](f: G ~> H): Coproduct[F, G, ?] ~> H =
    new (Coproduct[F, G, ?] ~> H) {
      def apply[A](c: Coproduct[F, G, A]): H[A] = c.run match {
        case -\/(fa) => nt(fa)
        case \/-(ga) => f(ga)
      }
    }
  }

  type -~>[F[_], G[_]] = Inject[F, G] 


  object LiftImplicit {
    implicit def lift[F[_], G[_], A](fa: F[A])(implicit I: F -~> G): Free.FreeC[G, A] = Free liftFC I.inj(fa)
  }
}


