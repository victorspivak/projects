object Matching {
    def main(args:Array[String]) {
        def testMatch1 (expr:List[Int]) {
            expr match {
                case List(0, a, b) => println("found it: " + a + " -> " + b)
                case List(0, a, b, _*) => println("found it: " + a + " -> " + b)
                case _ => println("Oops")
            }
        }

		def matchClass(obj:Any) {
			obj match {
				case s:String => println("It is a string")
				case s:Int => println("It is an int")
				case _ => println("It is unknown")
			}
		}

        testMatch1(List(0, 1, 1))
        testMatch1(List(0, 1, 1, 1, 0))

		matchClass(1)
		matchClass("1")
		matchClass(1.0)

        case object CaseObj

        check(CaseObj)
        check("1")
        def check(toCheck:AnyRef) {
            toCheck match {
                case CaseObj => println("Got it")
                case _ => println("Oops")
            }
        }
    }
}








