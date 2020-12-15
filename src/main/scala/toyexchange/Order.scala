package toyexchange

case class Order(id: Int,
                 createdAt: String,
                 symbol: String,
                 orderType: OrderType,
                 price: Float,
                 quantity: Int) {
  override def toString: String =
    s"#$id $createdAt $symbol ${orderType.toString} $price $quantity"
}
