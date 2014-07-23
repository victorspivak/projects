package svl.learn.scala.combinator

import scala.language.postfixOps
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object CalculatorWithVariablesX extends App {
  import svl.learn.scala.combinator.CalculatorWithVariablesX.Calculator.Vars

import scala.collection.mutable

  class Calculator(val variables:Vars) extends RegexParsers {
    def identificatorRegex = """[a-zA-Z]([a-zA-Z0-9])*""".r

    def number: Parser[Try[Double]] = """\d+(\.\d*)?""".r ^^ {v => Try(v.toDouble) }

    def variable: Parser[Try[Double]] = identificatorRegex ^^ {id: String =>
      Try(variables.get(id) match {
        case Some(value) => value
        case None =>
          throw new IllegalArgumentException(s"Error. Could not find variable $id")
      })
    }

    def numberOrVariable: Parser[Try[Double]] = number | variable
    def factor: Parser[Try[Double]] = numberOrVariable | "(" ~> expr <~ ")"
    def term  : Parser[Try[Double]] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
      case numberOrVariable ~ list => (numberOrVariable /: list) {
        case (x, "*" ~ y) => for {
          x_ <- x
          y_ <- y
        } yield x_ * y_
        case (x, "/" ~ y) => for {
          x_ <- x
          y_ <- y
        } yield x_ / y_
      }
    }

    //For enable logging use the following line
    //    def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    def expr  : Parser[Try[Double]] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case numberOrVariable ~ list => list.foldLeft(numberOrVariable) { // same as before, using alternate name for /:
        case (x, "+" ~ y) => for {
          x_ <- x
          y_ <- y
        } yield x_ + y_
        case (x, "-" ~ y) => for {
          x_ <- x
          y_ <- y
        } yield x_ - y_
      }
    }

    def assignment: Parser[String] = identificatorRegex ~ "=" ^^ {case (name ~ "=") => name}

    def statement: Parser[Try[Unit]] = assignment ~ expr ~ ";" ^^ {
      case name ~ value ~ semicolon =>
        value.map(variables.put(name, _))
    }

    def statements: Parser[Try[Vars]] = (statement *) ^^ { list =>
      val errors = list.filter(_.isFailure).foldLeft(new StringBuilder())((res, e) => res.append("\n").append(e.failed.get.getMessage))
      if (errors.isEmpty)
        Try(variables)
      else
        util.Failure(new Exception(errors.toString()))
    }

    def calculate(input: String): Try[Vars] = {
      parseAll(statements, input) match {
        case Success(result, _) => result
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

  def dump(res:Try[Vars]) {
    res match {
      case util.Success(v) => println(v)
      case util.Failure(e) => println(e.getMessage)
    }
    println("--------------------------------------------------------")
    println()
  }

  dump (Calculator("a=b; c=d;e=f;"))
  dump(Calculator(input))
  dump(Calculator(input, mutable.Map("z" -> 999)))
}
