package toyexchange

sealed trait OrderType

case object Buy extends OrderType
case object Sell extends OrderType

object OrderType {
  def fromValue(value: String): OrderType = value match {
    case "sell" => Sell
    case "buy"  => Buy
    case _      => throw new IllegalArgumentException(s"Illegal OrderType $value")
  }

  def matchType(value: OrderType): OrderType = value match {
    case Sell => Buy
    case Buy  => Sell
  }
}
