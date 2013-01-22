object AbstractTypes {
    def main(args:Array[String]) {
        trait Money  {
            type Currency <: Money
            val unit: String
            val amount: Double
            def compare(that:Currency):Int =  if(amount > that.amount) 1 else -1
            def +(that:Currency) = create(amount + that.amount)
            def -(that:Currency) = create(amount - that.amount)
            protected def create(amount:Double):Currency
        }

        trait OrderedMoney[T <: Money] extends Money with Ordered[T]

        case class Euro(amount: Double) extends OrderedMoney[Euro]{
            type Currency = Euro
            val unit = "EUR"
            def create(amount:Double) = Euro(amount)
        }

        case class Dollar(amount: Double) extends OrderedMoney[Dollar]{
            type Currency = Dollar
            val unit = "USD"
            def create(amount:Double) = Dollar(amount)
        }

        implicit def fromEuroToDollar(d: Euro) =  Dollar(d.amount * 1.2)
        implicit def fromDollarToEuro(d: Dollar) = Euro(d.amount * 0.85)
        implicit def fromDoubleToCurrency(d: Double) = new {
            def euro =  Euro(d)
            def dollar = Dollar(d)
        }

        //usage samples:
        assert(2.dollar + 3.euro >= 1.dollar + 1.dollar + 1.euro)

        println((2.dollar > 1.5.euro))
    }
}

