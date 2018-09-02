import cats.implicits._

object ArrowSyntax {

  def run = {
    val toLong:   Int   => Long   = _.toLong
    val toDouble: Float => Double = _.toDouble
    val toS:      Int   => String = "s" * _
    val ltoS:     Long  => String = x => s"s -> ${x}"

    val f1: ((Int, Float)) => (Long, Double) = toLong *** toDouble

    val f2: (Int) => (Long, String) = toLong &&& toS

    val f3: Int => String = toLong >>> ltoS

    val f4: Int => String = ltoS <<< toLong

    val f5: Either[Int, Long] => Either[String, String] = toS +++ ltoS
    val f6: Either[Int, Long] => String                 = toS ||| ltoS
  }
}
