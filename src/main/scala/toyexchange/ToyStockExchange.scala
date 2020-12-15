package toyexchange

class ToyStockExchange(allOrders: Seq[Order]) {

  def executeOrders: Seq[Trade] = {
    allOrders
      .groupBy(_.symbol)
      .map {
        case (_, orders) => matchAndTrade(orders, Seq.empty[Trade])
      }
      .toSeq
      .flatten
  }

  @scala.annotation.tailrec
  private def matchAndTrade(orders: Seq[Order],
                            trades: Seq[Trade]): Seq[Trade] =
    orders match {
      case Nil => trades
      case order :: otherOrders =>
        val matchType = OrderType.matchType(order.orderType)

        val matchingOrder =
          otherOrders
            .filter(_.orderType == matchType)
            .find(o => isHigherBid(order, o))

        matchingOrder match {
          case Some(o) =>
            val (newBuy, newSell, trade) = tradeBetween(order, o)

            val newOrders = orders
              .map(o => if (o.id == newBuy.id) newBuy else o)
              .map(o => if (o.id == newSell.id) newSell else o)
              .filter(_.quantity != 0)

            matchAndTrade(newOrders, trades :+ trade)
          case None => matchAndTrade(otherOrders, trades)
        }
    }

  private def isHigherBid(order1: Order, order2: Order): Boolean =
    (order1.orderType, order2.orderType) match {
      case (Buy, Sell) => order1.price >= order2.price
      case (Sell, Buy) => order2.price >= order1.price
      case _           => false
    }

  private def tradeBetween(order1: Order,
                           order2: Order): Tuple3[Order, Order, Trade] = {
    val (buy, sell) =
      (order1.orderType, order2.orderType) match {
        case (Buy, Sell) => (order1, order2)
        case (Sell, Buy) => (order2, order1)
        case _ =>
          throw new IllegalArgumentException(
            "Can't trade between orders of same type"
          )
      }

    val trade =
      Trade(buy.id, sell.price, Math.min(buy.quantity, sell.quantity), sell.id)

    (
      buy.copy(quantity = buy.quantity - trade.quantity),
      sell.copy(quantity = sell.quantity - trade.quantity),
      trade
    )
  }
}
