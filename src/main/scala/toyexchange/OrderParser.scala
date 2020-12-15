package toyexchange

import scala.util.{Failure, Success, Try}

class OrderParser(lines: Seq[String]) {

  def parse: Seq[Order] = {
    lines.flatMap { line =>
      dedupWhiteSpace(line) match {
        case s"#$id $createdAt $symbol $orderType $price $quantity" =>
          Try(
            Order(
              id.toInt,
              createdAt,
              symbol,
              OrderType.fromValue(orderType),
              price.toFloat,
              quantity.toInt
            )
          ) match {
            case Success(order) => Some(order)
            case Failure(e) =>
              println(e.getLocalizedMessage)
              None
          }
        case _ => None
      }
    }
  }

  private def dedupWhiteSpace(line: String): String =
    line.replaceAll(" +", " ").trim
}
