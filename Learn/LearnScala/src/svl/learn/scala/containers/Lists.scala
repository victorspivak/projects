object Lists {
    def main(args:Array[String]) {
        val oneTwo = List(1, 2)
        val threeFour = List(3, 4)
        val oneTwoThreeFour = oneTwo ::: threeFour

        println(oneTwo +" and "+ threeFour +" were not mutated.")
        println("Thus, "+ oneTwoThreeFour +" is a new list.")

        val twoThree = List(2, 3)
        val oneTwoThree = 1 :: twoThree
        println(oneTwoThree)

        println(1 :: 2 :: 3 :: Nil)

        Console println "count counts number of elements that match the expression" + oneTwoThreeFour.count(s => s > 1)
        Console println "drop returns a list without first n elements: " + oneTwoThreeFour.drop(2).count(_ > 1)
        Console println "dropRight returns a list without last n elements: " + oneTwoThreeFour.dropRight(2).count(_ > 1)

        Console println "exists: " + oneTwoThreeFour.exists(_ > 1) + "  "  + oneTwoThreeFour.exists(_ > 10)
        Console println "filter creates new list with elements that match the expression: " + oneTwoThreeFour.filter(_ > 1)
        Console println "forall validates do all elements match the expression: " + oneTwoThreeFour.forall(_ > 1) + "   " + oneTwoThreeFour.forall(_ > 0)

        Console print "foreach iterates all elements: "
        oneTwoThreeFour.foreach(s => Console print (" " + s))
        Console println ()

        Console println "head returns first element: " + oneTwoThreeFour.head + "  and tail returns list without first element: " + oneTwoThreeFour.tail
        Console println "last returns last element: " + oneTwoThreeFour.last + "  and init returns list without last element: " + oneTwoThreeFour.init
        Console println "isEmpty: " + oneTwoThreeFour.isEmpty + "  length: " + oneTwoThreeFour.length

        Console println "map creates new transformed using expression list: " + oneTwoThreeFour.map(_ * 2)
        Console println "mkString creates string with specified separator: " + oneTwoThreeFour.mkString(" : ")
        Console println "filterNot creates new list without elements that match the expression: " + oneTwoThreeFour.filterNot(_ > 1)
        Console println "reverse creates new list with reverse order: " + oneTwoThreeFour.reverse
        Console println "sortWith creates new list with reverse order: " + oneTwoThreeFour.sortWith((a,b) => a > b) + "   " + oneTwoThreeFour.sortWith((a,b) => a < b)

        val list = (1 to 10).toList
        Console println "FoldLeft: " + list.foldLeft (0)(_ + _)
        Console println "FoldRight: " + list.foldLeft (0)(_ + _)
        Console println "ReduceLeft: " + list.reduceLeft(_ + _)
        Console println "ReduceRight: " + list.reduceRight(_ + _)
        Console println "Fold: " + list.fold(0)(_ + _)
        Console println "Reduce: " + list.reduce(_ + _)
    }
}

