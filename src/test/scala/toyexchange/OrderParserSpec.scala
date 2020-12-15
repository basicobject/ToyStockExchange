package toyexchange

import org.scalatest.wordspec.AnyWordSpec

class OrderParserSpec extends AnyWordSpec {
  val inputLines =
    """
      |#1 09:45 TRUECALLER sell 240.12 100
      |#2 09:46 TRUECALLER sell 237.45  90
      |#3 09:47 TRUECALLER buy  238.10 110
      |#4 09:48 TRUECALLER buy  237.80  10
      |#5 09:49 TRUECALLER buy  237.80  40
      |#6 09:50 TRUECALLER sell 236.00  50
      |""".stripMargin

  val orders = Seq(
    Order(1, "09:45", "TRUECALLER", Sell, 240.12F, 100),
    Order(2, "09:46", "TRUECALLER", Sell, 237.45F, 90),
    Order(3, "09:47", "TRUECALLER", Buy, 238.10F, 110),
    Order(4, "09:48", "TRUECALLER", Buy, 237.80F, 10),
    Order(5, "09:49", "TRUECALLER", Buy, 237.80F, 40),
    Order(6, "09:50", "TRUECALLER", Sell, 236.00F, 50)
  )

  "OrderParser" when {
    "Given input lines" should {
      "parse the lines" in {
        val parser = new OrderParser(inputLines.split("\n"))
        assert(parser.parse == orders)
      }
    }

    "Given incorrect price" should {
      "not create an order object" in {
        val inputLines =
          """
            |#1 09:45 TRUECALLER sell 240.12 100
            |#2 09:46 TRUECALLER buy noprice  90
            |""".stripMargin

        val parser = new OrderParser(inputLines.split("\n"))
        assert(
          parser.parse ==
            Seq(Order(1, "09:45", "TRUECALLER", Sell, 240.12F, 100))
        )
      }
    }

    "Given incorrect order id" should {
      "not create an order object" in {
        val inputLines =
          """
            |#1 09:45 TRUECALLER sell 240.12 100
            |#b 09:46 TRUECALLER buy 230.09  90
            |""".stripMargin

        val parser = new OrderParser(inputLines.split("\n"))
        assert(
          parser.parse ==
            Seq(Order(1, "09:45", "TRUECALLER", Sell, 240.12F, 100))
        )
      }
    }

    "Given incorrect order type" should {
      "not create an order object" in {
        val inputLines =
          """
            |#1 09:45 TRUECALLER sell 240.12 100
            |#2 09:46 TRUECALLER run 230.08  90
            |""".stripMargin

        val parser = new OrderParser(inputLines.split("\n"))
        assert(
          parser.parse ==
            Seq(Order(1, "09:45", "TRUECALLER", Sell, 240.12F, 100))
        )
      }
    }

    "Given incorrect quantity" should {
      "not create an order object" in {
        val inputLines =
          """
            |#1 09:45 TRUECALLER sell 240.12 100
            |#2 09:46 TRUECALLER buy 230.08  ninety
            |""".stripMargin

        val parser = new OrderParser(inputLines.split("\n"))
        assert(
          parser.parse ==
            Seq(Order(1, "09:45", "TRUECALLER", Sell, 240.12F, 100))
        )
      }
    }

    "Given input with lot of whitespaces in the correct order" should {
      "create an order object" in {
        val inputLines =
          """
            |#1          09:45  TRUECALLER    sell 240.12    100
            |  #2   09:46   TRUECALLER  buy  230.08  90
            |""".stripMargin

        val parser = new OrderParser(inputLines.split("\n"))
        assert(
          parser.parse ==
            Seq(
              Order(1, "09:45", "TRUECALLER", Sell, 240.12F, 100),
              Order(2, "09:46", "TRUECALLER", Buy, 230.08F, 90)
            )
        )
      }
    }
  }
}
