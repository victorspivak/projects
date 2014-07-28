package svl.technology.subcut

import com.escalatesoft.subcut.inject.{Injectable, BindingModule}
import svl.technology.subcut.BindingKeys._

trait Greeting {
  def greet(name:String)
}

trait GreetingFormatter {
  def format(value:String):String
}

class GreetingTextFormatter extends GreetingFormatter{
  def format(value:String):String = value
}

class GreetingXmlFormatter extends GreetingFormatter{
  def format(value:String):String = s"<greeting>$value</greeting>"
}

trait GreetingService extends Greeting with Injectable{
  val bindings = implicitly[BindingModule]
  val greeting = injectOptional[String] (GreetingId) getOrElse {"How are you do,"}
  val formatter = injectOptional[GreetingFormatter] getOrElse new GreetingTextFormatter

  override def greet(name: String) {
    val formatted = formatter.format(greeting + " " + name)
    println(formatted)
  }
}

class GreetingModule (implicit val bindingModule:BindingModule) extends GreetingService with Injectable{
}
