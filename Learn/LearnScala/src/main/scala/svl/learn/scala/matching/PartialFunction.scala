package svl.learn.scala.matching

object PartialFunction {
    def main(args: Array[String]) {
        def fraction = new PartialFunction[Int, Int] {
            def apply(d: Int) = 64 / d
            def isDefinedAt(d: Int) = d != 0
        }

        exec(println(fraction(32)))
        exec(println(fraction(0)))
        println(fraction.isDefinedAt(32))
        println(fraction.isDefinedAt(0))

        def fraction1: PartialFunction[Int, Int] = {
            case d: Int if d != 0 => 64 / d
        }

        exec(println(fraction1(32)))
        exec(println(fraction1(0)))
        println(fraction1.isDefinedAt(32))
        println(fraction1.isDefinedAt(0))
    }

    def exec(f: =>Unit){
        try{
            f
        } catch{
            case e:Exception => println(e)
        }
    }
}
