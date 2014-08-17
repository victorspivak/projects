package svl.patterns.typesafebuilder

object ControlledBuilder extends App{
  trait Engine {
    def engine(value:String):this.type with Body
  }

  trait Body {this:CarBuilder =>
    def body(value:String):this.type with Transmission
  }

  trait Transmission {this:CarBuilder =>
    def transmission(value:String):this.type with CarBuilding
  }

  case class Car(engine:String, body:String, transmission:String)

  trait CarBuilding {this:CarBuilder =>
    def build = Car(engine, body, transmission)
  }

  class CarBuilder private extends Engine with Body with Transmission with CarBuilding{
    var engine:String = ""
    var body:String = ""
    var transmission:String = ""

    def engine(value:String) = {
      engine = value
      this
    }

    def body(value:String) = {
      body = value
      this
    }

    def transmission(value:String) = {
      transmission = value
      this
    }
  }

  object CarBuilder {
    def apply():Engine = new CarBuilder()
  }

  val builder = CarBuilder()
  val car= builder.engine("My Eng").body("My Body").transmission("My Trans").build

  println(car)
}
