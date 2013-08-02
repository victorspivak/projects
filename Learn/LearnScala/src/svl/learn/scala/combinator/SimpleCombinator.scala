import scala.util.parsing.combinator._

class Arith extends JavaTokenParsers {
    def expr: Parser[Any] = term~rep("+"~term | ""~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object ParseExpr extends Arith {
    def main(args: Array[String]) {
        val inp = "2 * (3 + 7)"
        println("input : "+ inp)
        val parsed = parseAll(expr, inp)

        println(parsed)

    }
}

