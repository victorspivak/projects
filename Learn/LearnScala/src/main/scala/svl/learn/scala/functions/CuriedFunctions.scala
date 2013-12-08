object CuriedFunctions {
    def main(args:Array[String]) {
        def plainOldSum(x: Int, y: Int) = x + y
        Console println plainOldSum (2,3)

        def curriedSum(x: Int)(y: Int) = x + y
        Console println curriedSum (2)(3)

        val onePlus = curriedSum(1)_
        Console println onePlus(3)
    }
}


