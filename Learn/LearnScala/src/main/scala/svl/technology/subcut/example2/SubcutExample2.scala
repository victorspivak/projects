package svl.technology.subcut.example2

import com.escalatesoft.subcut.inject.{BindingModule, NewBindingModule}

object SubcutExample2 extends App{
  implicit var bindings:BindingModule = MyBinding

  val greeter = bindings.injectOptional[GreetingService](None).getOrElse(new GreetingService("Privet", new GreetingTextFormatter))
  greeter.greet("Vic")

//  MyBinding.modifyBindings{implicit module =>
//    import module._
//    import BindingKeys._
//
//    bind[String] idBy GreetingId toModuleSingle(_ => "Hi")
//
//    val greeter1 = new GreetingModule()
//    greeter1.greet("Vic")
//  }
//
//  object MyBinding1 extends NewBindingModule(module => {
//    import module._
//    import BindingKeys._
//
//    bind[String] idBy GreetingId toModuleSingle(_ => "Good Morning")
//  })
//
//  bindings = MyBinding1 andThen MyBinding
//
//  val greeter1 = new GreetingModule()
//  greeter1.greet("Victor")
}
