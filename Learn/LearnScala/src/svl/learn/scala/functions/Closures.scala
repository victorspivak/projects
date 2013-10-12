package svl.learn.scala.functions

object Closures {
	def main(args: Array[String]) {
		def foo (printFunc:String=>Unit) {
			printFunc("Hello")
		}

		foo(println)
	}
}
