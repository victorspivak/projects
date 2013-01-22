object BasicClasses {
    def main(args:Array[String]) {
        val hello : Any = "Hello"
        Console println  hello.hashCode
        Console println  hello.##
        Console println  (hello ##)
    }
}

