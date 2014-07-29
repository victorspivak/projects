package svl.technology.subcut.example2

import com.escalatesoft.subcut.inject.{Injectable, BindingModule, NewBindingModule}

object SubcutExample2 extends App with Injectable{
  implicit var bindingModule:BindingModule = MyBinding2

  val greeter = inject[Greeting]
  greeter.greet("Vic")

  MyBinding2.modifyBindings{implicit module =>
    import BindingKeys._

    module.bind[String] idBy GreetingId toModuleSingle(_ => "Hi")
    bindingModule = module
    val greeter1 = inject[Greeting]
    greeter1.greet("Vic")
  }

  object MyBinding21 extends NewBindingModule(module => {
    import module._
    import BindingKeys._

    bind[String] idBy GreetingId toModuleSingle(_ => "Good Morning")
  })

  bindingModule = MyBinding21 andThen MyBinding2

  val greeter1 = inject[Greeting]
  greeter1.greet("Victor")

  new GreetingService(Some("Privet"), Some(new GreetingTextFormatter)).greet("Victor")
  new GreetingService(Some("Privet")).greet("Victor")
}
