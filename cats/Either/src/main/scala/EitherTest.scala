import cats.implicits._

case class DataFrameHolder(sql: Option[String])
case class SqlDataSource(dfh:   Option[DataFrameHolder])
case class Validation(sql:      Option[SqlDataSource])

object EitherTest {
  val dataFrameHolder = DataFrameHolder(None)
  //val dataFrameHolder = DataFrameHolder("select * from test".some)
  val sqlDataSource = SqlDataSource(dataFrameHolder.some)
  val validation    = Validation(sqlDataSource.some)

  def runOption = {
    val result = for {
      sqlDataSource   <- validation.sql
      dataFrameHolder <- sqlDataSource.dfh
      sql             <- dataFrameHolder.sql
    } yield sql
    println(result)

  }

  def toEither[T](s: Option[T], error: String) = {
    s.liftTo[Either[String, ?]](error)
  }

  def runEither = {

    val result =
      for {
        sqlDataSource   <- toEither(validation.sql, s"Missing sql container")
        dataFrameHolder <- toEither(sqlDataSource.dfh, s"dfh missing sql")
        sql             <- toEither(dataFrameHolder.sql, s"Missing sql")
      } yield sql
    result match {
      case Right(r) => r
      case Left(e)  => throw new Exception(e)
    }
  }

}
