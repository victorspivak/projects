package svl.patterns.typesafebuilder

object ControlledBuilder extends App{
  trait Engine {this:CarBuilder =>
    var engine:String = ""

    def engine(value:String):this.type with Body = {
      engine = value
      this
    }
  }

  trait Body {this:CarBuilder =>
    var body:String = ""

    def body(value:String):this.type with Transmission = {
      body = value
      this
    }
  }

  trait Transmission {this:CarBuilder =>
    var transmission:String = ""

    def transmission(value:String):this.type with CarBuilding = {
      transmission = value
      this
    }
  }

  case class Car(engine:String, body:String, transmission:String)

  trait CarBuilding {this:CarBuilder =>
    def build = Car(engine, body, transmission)
  }

  class CarBuilder private extends Engine with Body with Transmission with CarBuilding
  object CarBuilder {
    def apply():Engine = new CarBuilder()
  }

  val builder = CarBuilder()

  val car= builder.engine("My Eng").body("My Body").transmission("My Trans").build

  println(car)
}
