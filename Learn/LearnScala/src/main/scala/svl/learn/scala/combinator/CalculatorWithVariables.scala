package svl.learn.scala.combinator

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object CalculatorWithVariables extends App {
  import svl.learn.scala.combinator.CalculatorWithVariables.Calculator.Vars

  import scala.collection.mutable

  class Calculator(val variables:Vars) extends RegexParsers {
    def identificatorRegex = """[a-zA-Z]([a-zA-Z0-9])*""".r

    def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ {_.toDouble}
    def variable: Parser[Double] = identificatorRegex ^^ {id: String =>
      variables.get(id) match {
        case Some(value) => value
        case None =>
          throw new IllegalArgumentException(s"Error. Could not find variable $id")
      }
    }
    def numberOrVariable: Parser[Double] = number | variable
    def factor: Parser[Double] = numberOrVariable | "(" ~> expr <~ ")"
    def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
      case numberOrVariable ~ list => (numberOrVariable /: list) {
        case (x, "*" ~ y) => x * y
        case (x, "/" ~ y) => x / y
      }
    }

//For enable logging use the following line
//    def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    def expr  : Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case numberOrVariable ~ list => list.foldLeft(numberOrVariable) { // same as before, using alternate name for /:
        case (x, "+" ~ y) => x + y
        case (x, "-" ~ y) => x - y
      }
    }

    def assignment: Parser[String] = identificatorRegex ~ "=" ^^ {case (name ~ "=") => name}

    def statement: Parser[Unit] = assignment ~ expr ~ ";" ^^ {
      case name ~ value ~ semicolon =>
        variables.put(name, value)
    }

    def statements: Parser[Vars] = statement ~ rep(statement) ^^ {
      case name ~ list => variables
    }

    def calculate(input: String): Try[Vars] =
      Try{
        parseAll(statements, input) match {
          case Success(result, _) => variables
          case failure: NoSuccess => scala.sys.error(failure.msg)
        }
      }
    }

  object Calculator {
    type Vars = mutable.Map[String, Double]
    def apply(input: String): Try[Vars] = new Calculator(mutable.Map()).calculate(input)
    def apply(input: String, vars:Vars): Try[Vars] = new Calculator(vars).calculate(input)
  }

  val input =
    """
      |abc = 5*5 + 3*4;
      |a = 10;
      |b = abc*a + abc + a;
      |c = z;
    """.stripMargin

  val res1 = Calculator(input)
  println(res1)

  val res2 = Calculator(input, mutable.Map("z" -> 999))
  println(res2)
}
