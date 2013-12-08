import scala.util.parsing.combinator._

object ParseJSON extends JSON {
    def main(args: Array[String]) {
        val js = """{
                   "address book": {
                   "name": "John Smith",
                   "address": {
                   "street": "10 Market Street",
                   "city" : "San Francisco, CA",
                   "zip" : 94111
                   },
                   "phone numbers": [
                   "408 3384238",
                   "408 1116892"
                   ]
                   }
                   }"""
        val parsed = parseAll(value, js)
        println(parsed)



    }
}




class JSON extends JavaTokenParsers {
    def value : Parser[Any] = obj | arr |
        stringLiteral |
        floatingPointNumber |
        "null" | "true" | "false"
    def obj : Parser[Any] = "{"~repsep(member, ",")~"}"
    def arr : Parser[Any] = "["~repsep(value, ",")~"]"
    def member: Parser[Any] = stringLiteral~":"~value
}
