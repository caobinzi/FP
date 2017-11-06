


  import scalaz._
  import Scalaz._
  val double:Int => Int = 2*

  //Functor
  Option(2).map(double)  
  //Applicative 
  Option(2) <*> Option(double) 
  //Functor
  Option(2) >>= (double(_).some)


