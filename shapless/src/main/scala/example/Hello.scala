package example
import shapeless._
import record._, syntax.singleton._

case class Book(author:         String, title: String, id: Int, price: Double)
case class ExtendedBook(author: String, title: String, id: Int, price: Double, inPrint: Boolean)
import labelled.FieldType

trait Field {
  type K
  type V
  type F = FieldType[K, V]
}

object Field {
  def apply[K0, V0](sample: FieldType[K0, V0]) = new Field { type K = K0; type V = V0 }
}

object Hello extends App {
  def lableGenericTest = {

    val bookGen    = LabelledGeneric[Book]
    val bookExtGen = LabelledGeneric[ExtendedBook]

    val tapl =
      ExtendedBook("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11, false)

    val rec = bookExtGen.to(tapl) // Convert case class value to generic representation

    rec('price) // Access the price field symbolically, maintaining type information

    val book = bookGen.from(rec - 'inPrint)
    println(book)

  }
  def alignTest = {
    import ops.hlist.Align
    import syntax.singleton._

    case class From(s1: String, s2: String)
    case class To(i:    Int, s2:    String, s1: String)

    val from = From("foo", "bar")

    val fromGen = LabelledGeneric[From]
    val toGen   = LabelledGeneric[To]

    // Define the type of the i field by example
    val iField = Field('i ->> 0)

    val align = Align[iField.F :: fromGen.Repr, toGen.Repr]

    val to = toGen.from(align('i ->> 23 :: fromGen.to(from)))
    println(to)
    println
  }
  alignTest

}
