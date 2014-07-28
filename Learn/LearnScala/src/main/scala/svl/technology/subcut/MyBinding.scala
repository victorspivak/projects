package svl.technology.subcut

import com.escalatesoft.subcut.inject.{BindingId, NewBindingModule}
import svl.learn.scala.akka.Greeter

object BindingKeys {
  object GreetingId extends BindingId
  object MaxTryCountId extends BindingId
}

object MyBinding extends NewBindingModule(module => {
  import module._
  import BindingKeys._

  bind[Int] idBy MaxTryCountId toSingle  100
  bind[String] idBy GreetingId toModuleSingle(_ => "Hello")
  bind[GreetingFormatter] toModuleSingle{implicit module => new GreetingXmlFormatter()}
})
