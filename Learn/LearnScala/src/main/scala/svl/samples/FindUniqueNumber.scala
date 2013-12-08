object FindUniqueNumber {
    def main(args:Array[String]) {
        val values = List(1,5,7,-4, 1,7,-4)

        val res = values.foldLeft(0)(_^_)
        println(res)

        println(Integer.MAX_VALUE ^ Integer.MAX_VALUE)
        println(Integer.MIN_VALUE ^ Integer.MIN_VALUE)
        println(Integer.MAX_VALUE ^ Integer.MIN_VALUE ^ Integer.MAX_VALUE ^ Integer.MIN_VALUE)
    }
}

