import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers


object CalculatorUsingStdTokenParser extends StdTokenParsers with App {
  type Tokens = StdLexical

  val lexical = new StdLexical

  lexical.delimiters ++= List("(", ")", "+", "-", "*", "/")

  def factor: Parser[Int] = "(" ~> expr <~ ")" | numericLit ^^ (_.toInt)

  def term: Parser[Int] = factor ~ "*" ~ term ^^ { case x ~ "*" ~ y => x * y} |
    factor ~ "/" ~ term ^^ { case x ~ "/" ~ y => x / y} | factor

  def expr: Parser[Int] = term ~ "+" ~ expr ^^ { case x ~ "+" ~ y => x + y} |
    term ~ "-" ~ expr ^^ { case x ~ "-" ~ y => x - y} | term

  Console.println(expr(new lexical.Scanner("1+2*3*7-1")))
}

