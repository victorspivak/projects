object Rational {
    def main(args:Array[String]) {
        class Rational(n: Int, d: Int) {
            require(d != 0)
            private val g = gcd(n.abs, d.abs)
            val numer = n / g
            val denom = d / g

            def this(n: Int) = this (n, 1)

            def +(that: Rational): Rational =
                new Rational(
                    numer * that.denom + that.numer * denom,
                    denom * that.denom
                )

            def +(i: Int): Rational =
                new Rational(numer + i * denom, denom)

            def -(that: Rational): Rational =
                new Rational(
                    numer * that.denom - that.numer * denom,
                    denom * that.denom
                )
            def -(i: Int): Rational = new Rational(numer - i * denom, denom)

            def *(that: Rational): Rational =
                new Rational(numer * that.numer, denom * that.denom)

            def *(i: Int): Rational =
                new Rational(numer * i, denom)

            def /(that: Rational): Rational =
                new Rational(numer * that.denom, denom * that.numer)

            def /(i: Int): Rational =
                new Rational(numer, denom * i)

            override def toString = numer + "/" + denom

            private def gcd(a: Int, b: Int): Int =
                if (b == 0) a else gcd(b, a % b)
        }
        implicit def intToRational(x: Int) = new Rational(x)

        try {
            val invalid = new Rational(1, 0)
        } catch {
            case e: IllegalArgumentException => Console println "Got expected exception"
        }

        val oneHalf = new Rational(1, 2)
        val oneThird = new Rational(1, 3)
        val twoThird = new Rational(2, 3)
        Console println oneHalf
        Console println oneThird
        Console println (oneHalf + oneThird)
        Console println (oneHalf * twoThird)
        Console println (twoThird * 2)
        Console println (2 * twoThird)

        def testFinally (d : Int) = try {
            val invalid = new Rational(1, d)
            1
        } catch {
            case e: IllegalArgumentException => Console println "Got expected exception"
            2
        }
        finally {
            println ("I am in finally")
        }

        Console println testFinally(1)
    }
}

