package svl.learn.scala.combinator

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

object ScalaCodeParser  extends RegexParsers{
  val ID:Parser[String] = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r ^^ {s => s}

  val NUM = """[1-9][0-9]*""".r

  def formals = repsep(ID ~ ":" ~ ID, ",")
  def formalsList = "(" ~ formals ~ ")"

  def actuals = expr *
  def actualsList = "(" ~ actuals ~ ")"

  def classPrefix = "class" ~ ID ~ opt(formalsList)

  def classExt = "extends" ~ ID ~ opt(actualsList)

  def member = (
      "val" ~ ID ~ ":" ~ ID ~ "=" ~ expr
    | "var" ~ ID ~ ":" ~ ID ~ "=" ~ expr
    | "def" ~ ID ~ opt(formalsList) ~ ":" ~ ID ~ "=" ~ expr ^^
        {case _ ~ name ~ params ~ _ ~ retType ~ _ ~ expr => ("def", name, params, retType, expr)}
    | "type" ~ ID ~ "=" ~ ID ^^
        {case _ ~ alias ~ _ ~ newType => ("type", alias, newType)}
  )

  def expr: Parser[Any] = (factor ~ (
    "+" ~ factor
      | "-" ~ factor
    )).*

  def factor = term ~ ("." ~ ID ~ "(" ~ actuals ~ ")").*

  def term = (
      "(" ~ expr ~ ")"
    | ID
    | NUM
  )

  def body = "{" ~ (member *) ~ "}" ^^ {
    case o ~ members ~ c => members
  }

  def clazz = classPrefix ~ opt(classExt) ~ opt(body)

  def program = clazz *

  def apply(input: String) = parseAll(program, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}

object ParseScalaCodeParser extends App {
  val source =
    """
      |class Foo(name:String, id:Int){
      |def doIt(a:Int, b:Int):Int = a+b
      |}
      |class Bar extends Foo{
      |  type MyString = String
      |}
      |""".stripMargin

  import ScalaCodeParser._
  val rez = ScalaCodeParser(source)
  println(rez)
  println()
  println("Classes:")
  rez.foreach{
    case ("class" ~ className ~ clParams) ~ clExt ~ clBodyOpt => println(s"class $className")
      clBodyOpt map{member=>

        member.foreach{
          case ("def", name, params, retType, expr) => println(s"\t$name")
          case ("type", alias, newType) => println(s"\t$alias")
        }
      }
  }
}
