//package svl.web
//
//object RandomString {
//
//}
//
//object Test extends App{
//  import net.liftweb.json.DefaultFormats
//  import net.liftweb.json.JsonParser._
//  implicit val formats = DefaultFormats
//
//  val json="{\"vendor\": \"ABC\", \"amount\": 1000}"
//  case class Inv(vendor:String, amount:Int)
////  object Inv{
////    def apply(vendor:String, amount:Int) = new Inv(vendor,amount)
////  }
//
//
//
//  val inv = parse(json).extract[Inv]
//  println(inv)
//
//
//
//}
