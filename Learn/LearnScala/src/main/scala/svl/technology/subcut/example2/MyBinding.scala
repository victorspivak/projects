package svl.technology.subcut.example2

import com.escalatesoft.subcut.inject._


object BindingKeys {
  val injected: Option[Nothing] = None

  object GreetingId extends BindingId
  object MaxTryCountId extends BindingId
}

object MyBinding2 extends NewBindingModule(module => {
  import module._
  import BindingKeys._

  bind[Int] idBy MaxTryCountId toSingle  100
  bind[String] idBy GreetingId toModuleSingle(_ => "Hello")
  bind[GreetingFormatter] toModuleSingle{implicit module => new GreetingXmlFormatter()}
  bind[Greeting] toProvider  {implicit module => new GreetingService()}
})
