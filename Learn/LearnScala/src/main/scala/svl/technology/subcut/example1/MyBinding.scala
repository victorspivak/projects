package svl.technology.subcut.example1

import com.escalatesoft.subcut.inject.{BindingId, NewBindingModule}

object BindingKeys {
  object GreetingId extends BindingId
  object MaxTryCountId extends BindingId
}

object MyBinding extends NewBindingModule(module => {
  import module._
  import svl.technology.subcut.example1.BindingKeys._

  bind[Int] idBy MaxTryCountId toSingle  100
  bind[String] idBy GreetingId toModuleSingle(_ => "Hello")
  bind[GreetingFormatter] toModuleSingle{implicit module => new GreetingXmlFormatter()}
})
