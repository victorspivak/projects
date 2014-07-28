package svl.technology.subcut

import com.escalatesoft.subcut.inject.{BindingModule, NewBindingModule}

object SubcutExample extends App{
  implicit var bindings:BindingModule = MyBinding
  val greeter = new GreetingModule()
  greeter.greet("Vic")

  MyBinding.modifyBindings{implicit module =>
    import module._
    import BindingKeys._

    bind[String] idBy GreetingId toModuleSingle(_ => "Hi")

    val greeter1 = new GreetingModule()
    greeter1.greet("Vic")
  }

  object MyBinding1 extends NewBindingModule(module => {
    import module._
    import BindingKeys._

    bind[String] idBy GreetingId toModuleSingle(_ => "Good Morning")
  })

  bindings = MyBinding1 andThen MyBinding

  val greeter1 = new GreetingModule()
  greeter1.greet("Victor")
}
