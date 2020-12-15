import toyexchange.{Order, OrderParser, OrderType, ToyStockExchange}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main extends App {

  args.headOption match {
    case Some(filename) =>
      readOrders(filename) match {
        case Right(lines) =>
          val orders = new OrderParser(lines).parse

          new ToyStockExchange(orders).executeOrders
            .foreach(println)
        case Left(exception) => exception.printStackTrace()
      }
    case None => println("Please provide the order input file as argument.")
  }

  private def readOrders(filename: String): Either[Throwable, Seq[String]] =
    Try(Source.fromFile(filename)) match {
      case Success(file)      => Right(file.getLines().toSeq)
      case Failure(exception) => Left(exception)
    }
}
