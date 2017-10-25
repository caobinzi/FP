import scalaz._
import scalaz.Scalaz._
import scala.reflect.runtime.{universe => ru}
//Some test about how to using reflection in scala
object Reflect {
  def getSymbols[T: ru.TypeTag]: List[ru.Symbol] = ru.typeOf[T].members.toList

  def getSymbols(obj: Any): List[ru.Symbol] =
    ru.runtimeMirror(getClass.getClassLoader).reflect(obj).symbol.toType.members.toList

  def hasType[T2: ru.TypeTag](obj: Any, s: String): Boolean =
    getSymbols(obj).exists { x =>
      (!x.isMethod) &&
      (x.name.toString.trim === s) &&
      x.typeSignature =:= ru.typeOf[T2]
    }

  def getFields(obj:      Any): List[ru.Symbol] = getSymbols(obj).filter(!_.isMethod)
  case class Person(name: String, age: Int)
  getSymbols[Person].filter(!_.isMethod).map(x => x.name -> x.info)
}
