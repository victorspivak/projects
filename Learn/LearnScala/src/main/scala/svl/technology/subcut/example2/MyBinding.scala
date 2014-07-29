package svl.technology.subcut.example2

import com.escalatesoft.subcut.inject.{BindingModule, NewBindingModule, BindingId}


object BindingKeys {
  object GreetingId extends BindingId
  object MaxTryCountId extends BindingId
}

object MyBinding extends NewBindingModule (module => {
  import module._
  import BindingKeys._

  bind[Int] idBy MaxTryCountId toSingle  100
  bind[String] idBy GreetingId toModuleSingle(_ => "Hello")
  bind[GreetingFormatter] toModuleSingle{implicit module => new GreetingXmlFormatter()}
  bind[Greeting] toModuleSingle{implicit m:BindingModule =>
    val greeting = m.injectOptional[String] (Some(GreetingId.toString)) getOrElse {"How are you do,"}
    val formatter =m.injectOptional[GreetingFormatter](None) getOrElse new GreetingTextFormatter

    new GreetingService(greeting, formatter)
  }
})
