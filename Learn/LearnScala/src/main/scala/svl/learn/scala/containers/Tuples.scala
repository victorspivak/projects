object Tuples {
  def main(args: Array[String]) {
    val pair = (99, "Luftballons")
    Console println pair._1 + " -> " + pair._2

    val t = (10, 20, 30, 40)

    t.productIterator.foreach(println)

    println("===============================================================")
    println("Arity: " + t.productArity)
    for (i <- 0 until t.productArity)
      println(s"$i -> ${t.productElement(i)}")
    println("===============================================================")
    list2TuppleHack()
  }

  def list2TuppleHack() {
    def toTuple[A <: Object](as:List[A]):Product = {
      val tupleClass = Class.forName("scala.Tuple" + as.size)
      tupleClass.getConstructors.apply(0).newInstance(as:_*).asInstanceOf[Product]
    }

    val rec1 = (Some(1), None, None, Some("Hello"), None, Some("World"))
    val l = rec1.productIterator.asInstanceOf[Iterator[Option[Object]]].filter{_.isDefined}.foldLeft(List[Object]()){(l,e) => e :: l}.reverse
    val rec2 = toTuple(l)

    println(rec1)
    println(rec2)
  }
}




