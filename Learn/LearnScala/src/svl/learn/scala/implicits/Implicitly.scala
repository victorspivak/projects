package svl.learn.scala.implicits

object Implicitly extends App{
	def guessLuckyNumber1(n:Int) = {
		if (n == 0)
     		implicitly[Int]
		else
			n
	 }

	println(guessLuckyNumber1(0))

    implicit val luckyNumber = 5

	println(guessLuckyNumber1(0))

    def guessLuckyNumber() = {
        implicitly[Int]
    }

    def guessLuckyNumberWithImplicitParameter(implicit n:Int) = {
        n
    }

    println (guessLuckyNumber())
    println (guessLuckyNumberWithImplicitParameter)



	def less1[A <% Ordered[A]] (pair:(A, A)) = pair._1 < pair._2

	def less2[A] (pair:(A, A)) (implicit aIsOrdered: A => Ordered[A]) = pair._1 < pair._2


	println(less1((5, 10)))
	println(less2((5, 10)))

	trait Reads[A]{
		def read(bytes:Array[Byte]): A
	}

	object Reads{
		implicit object IntReads extends Reads[Int] {
			override def read(bytes: Array[Byte]): Int = 555
		}

		def read[A](bytes: Array[Byte]) (implicit ev: Reads[A]) : A =
			ev.read(bytes)
	}

//	def read[A](bytes: Array[Byte]) (implicit ev: Reads[A]) : A =
//		ev.read(bytes)
//
//	def read[A : Reads](bytes: Array[Byte]) : A = {
//		val ev = implicitly[Reads[A]]
//		ev.read(bytes)
//	}

	println(Reads.read[Int](null))

	implicit object customIntRead extends Reads[Int]{
		def read(bytes: Array[Byte]): Int = 8888
	}

	println(Reads.read[Int](null))
}
