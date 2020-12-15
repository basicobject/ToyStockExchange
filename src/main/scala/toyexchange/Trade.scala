package toyexchange

case class Trade(buyOrderId: Int,
                 sellPrice: Float,
                 quantity: Int,
                 sellOrderId: Int) {
  override def toString: String =
    s"#$buyOrderId $sellPrice $quantity #$sellOrderId"
}
