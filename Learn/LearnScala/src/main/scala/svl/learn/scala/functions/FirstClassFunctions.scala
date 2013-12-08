object FirstClassFunctions {
    def main(args:Array[String]) {
        var increase = (x:Int) => x + 1

        Console println increase(10)

        increase = (x:Int) => x * 2

        Console println increase(10)

        var incrementBy = 1
        increase = (x:Int) => x + incrementBy
        Console println increase(10)

        incrementBy = 2
        Console println increase(10)
    }
}


