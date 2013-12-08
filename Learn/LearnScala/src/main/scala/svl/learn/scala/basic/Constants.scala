package svl.learn.scala.basic

object Constants {
    def main(args:Array[String]) {
		Case.doIt()
    }

	object Case {
	  val lowerConst = "lower"
	  val UpperConst = "UPPER"

	  def doIt() {
	    for (i <- Seq(lowerConst, UpperConst, "should mismatch.").map(Option.apply)) {
	      print("Input '%s' results in: ".format(i))
	      i match {
	        case Some(UpperConst) => println("UPPER!!!")
	        case Some(lowerConst) => println("lower!")         // Be careful with naming
	        case _ => println("mismatch!")
	      }
	    }
	  }
	}
}

