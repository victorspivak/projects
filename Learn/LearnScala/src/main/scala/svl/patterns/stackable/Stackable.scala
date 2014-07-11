package svl.patterns.stackable

object Stackable {
  abstract class IntQueue {
      def get(): Int
      def put(x: Int)
    }

  trait Doubling extends IntQueue {
     abstract override def put(x: Int) { super.put(2 * x) }
   }

  trait Incrementing extends IntQueue {
      abstract override def put(x: Int) { super.put(x + 1) }
    }

  trait Filtering extends IntQueue {
     abstract override def put(x: Int) {
       if (x >= 0) super.put(x)
     }
   }

  trait Logger {
     def log(msg:String):Unit = println(msg)
   }

  trait Logging extends IntQueue {
     this:Logger =>
     abstract override def put(x: Int) {
       log("put: " + x)
       super.put(x)
     }
   }

  import scala.collection.mutable.ArrayBuffer

   class BasicIntQueue extends IntQueue {
     private val buf = new ArrayBuffer[Int]
     def get() = buf.remove(0)
     def put(x: Int) { buf += x }
   }

  def main(args: Array[String]) {
    val collection = new BasicIntQueue with Doubling with Incrementing with Logger with Logging

    collection.put(10)
    collection.put(-10)
    collection.put(11)

    println(collection.get())
    println(collection.get())
    println(collection.get())
  }

}
