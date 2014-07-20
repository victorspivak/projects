package svl.learn.scala.combinator

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object CalculatorWithVariables extends App {
  import svl.learn.scala.combinator.CalculatorWithVariables.Calculator.Vars

  import scala.collection.mutable.Map

  class Calculator(val variables:Vars) extends RegexParsers {

    //  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
    //  def factor: Parser[Double] = number | "(" ~> expr <~ ")"
    //  def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
    //    case number ~ list => (number /: list) {
    //      case (x, "*" ~ y) => x * y
    //      case (x, "/" ~ y) => x / y
    //    }
    //  }

    def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ {
      _.toDouble
    }

    def variable: Parser[String] = """[a-zA-Z]([a-zA-Z0-9])*""".r ^^ { id: String => id}

    def assignment: Parser[String] = variable ~ "=" ^^ {
      _._1
    }

    def statement: Parser[Double] = assignment ~ number ~ ";" ^^ {
      case name ~ value ~ semicolon =>

        println(name + " -> " + value)
        value
    }

    def statements: Parser[Double] = statement ~ rep(statement) ^^ {
      case name ~ list => 5.0
    }

    ////  def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    //  def expr  : Parser[Double] = variable ~ "=" ~ number ^^ {
    //    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
    //      case (x, "+" ~ y) => x + y
    //      case (x, "-" ~ y) => x - y
    //    }
    //  }

    def calculate(input: String): Double = parseAll(statements, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }

  object Calculator {
    type Vars = mutable.Map[String, Double]
    def apply(input: String): Double = new Calculator(mutable.Map()).calculate(input)
  }

  val input =
    """
      |abc = 5;
      |a = 10;
    """.stripMargin
  println(Calculator(input))
}

//  type Vars = Map[String, Double]
//
//  //  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
//  //  def factor: Parser[Double] = number | "(" ~> expr <~ ")"
//  //  def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
//  //    case number ~ list => (number /: list) {
//  //      case (x, "*" ~ y) => x * y
//  //      case (x, "/" ~ y) => x / y
//  //    }
//  //  }
//
//  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
//  def variable: Parser[String] = """[a-zA-Z]([a-zA-Z0-9])*""".r ^^ {id:String => id}
//  def assignment: Parser[String] = variable ~ "=" ^^ {_._1}
//  def statement  : Parser[Double] = assignment ~ number ~ ";" ^^ {
//    case name ~ value ~ semicolon =>
//
//      println(name)
//      value
//  }
//
//  def statements : Parser[Double] = statement ~ rep(statement)  ^^ {
//    case name ~ list => 5.0
//  }
//
//  ////  def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
//  //  def expr  : Parser[Double] = variable ~ "=" ~ number ^^ {
//  //    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
//  //      case (x, "+" ~ y) => x + y
//  //      case (x, "-" ~ y) => x - y
//  //    }
//  //  }
//
//  def apply(input: String): Double = parseAll(statements, input) match {
//    case Success(result, _) => result
//    case failure : NoSuccess => scala.sys.error(failure.msg)
//  }
//
//  val input =
//    """
//      |abc = 5;
//      |a = 10;
//    """.stripMargin
//  println(apply(input))
//}

//object CalculatorWithVariables extends RegexParsers with App{
//  type Vars = Map[String, Double]
//
////  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
////  def factor: Parser[Double] = number | "(" ~> expr <~ ")"
////  def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
////    case number ~ list => (number /: list) {
////      case (x, "*" ~ y) => x * y
////      case (x, "/" ~ y) => x / y
////    }
////  }
//
//  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
//  def variable: Parser[String] = """[a-zA-Z]([a-zA-Z0-9])*""".r ^^ {id:String => id}
//  def assignment: Parser[String] = variable ~ "=" ^^ {_._1}
//  def statement  : Parser[Double] = assignment ~ number ~ ";" ^^ {
//    case name ~ value ~ semicolon =>
//
//      println(name)
//      value
//  }
//
//  def statements : Parser[Double] = statement ~ rep(statement)  ^^ {
//    case name ~ list => 5.0
//  }
//
//////  def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
////  def expr  : Parser[Double] = variable ~ "=" ~ number ^^ {
////    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
////      case (x, "+" ~ y) => x + y
////      case (x, "-" ~ y) => x - y
////    }
////  }
//
//  def apply(input: String): Double = parseAll(statements, input) match {
//    case Success(result, _) => result
//    case failure : NoSuccess => scala.sys.error(failure.msg)
//  }
//
//  val input =
//    """
//      |abc = 5;
//      |a = 10;
//    """.stripMargin
//  println(apply(input))
//}
//
//  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
//  def factor: Parser[Double] = number | "(" ~> expr <~ ")"
//  def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
//    case number ~ list => (number /: list) {
//      case (x, "*" ~ y) => x * y
//      case (x, "/" ~ y) => x / y
//    }
//  }
//  def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
//    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
//      case (x, "+" ~ y) => x + y
//      case (x, "-" ~ y) => x - y
//    }
//  }
//
//  def apply(input: String): Double = parseAll(expr, input) match {
//    case Success(result, _) => result
//    case failure : NoSuccess => scala.sys.error(failure.msg)
//  }

