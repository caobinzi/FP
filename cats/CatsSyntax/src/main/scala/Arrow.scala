import cats.implicits._

object ArrowSyntax {

  def run = {

    val toLong:   String => Long   = _.toLong
    val toInt:    String => Int    = _.toInt
    val toDouble: String => Double = _.toDouble

    val longToString: Long => String = x => s"s -> ${x}"
    val intToString:  Int  => String = x => s"s -> ${x}"

    /*
     *Arrow
     *  A---->B  C---->D ===> (A, C) => (B, D)
     *  A---->B  A---->C ===> (A) => (B, C)
     */
    val f1: ((String, String)) => (Long, Double) = toLong *** toDouble
    val f2: String             => (Long, Int)    = toLong &&& toInt

    /*
     *Compose
     *  A---->B  C---->D ===> A => D
     */
    //Compose
    val f3: String => String = toLong >>> longToString
    val f4: String => String = longToString <<< toLong

    /*
     *Arrow Choice
     *  A---->B  C---->D ===> Either[A, C] => Either[B, D]
     */

    val f5: Either[String, String] => Either[Int, Long] = toInt +++ toLong

    /*
     *Choice
     *  A---->C  B---->C ===> Either[A, B] => C
     */
    val f6: Either[Int, Long] => String = intToString ||| longToString

  }
}
