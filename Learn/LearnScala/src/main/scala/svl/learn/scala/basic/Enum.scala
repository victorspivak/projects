object Enum {
    def main(args:Array[String]) {
        println ("Enums:")

        object WeekDay extends Enumeration {

            type WeekDay = Value
            val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
        }
        import WeekDay._

        def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)

        WeekDay.values filter isWorkingDay foreach println

        object Colors extends Enumeration {
            type Colors = Value
            val Black, White, Red, Green, Yellow, Blue = Value
        }

        object TrafficColors extends Enumeration {
            type TrafficColors = Value
            val Red, Green, Yellow = Value
        }

        import TrafficColors._

        implicit def TrafficColor2Color(color: TrafficColors) = Colors.withName(color.toString)

        object ColorProcessor {
            import Colors._

            def fun1(color:Colors) {
                println(color)
            }
        }

        def fun2(color:TrafficColors) {
            ColorProcessor.fun1(color)
        }

        fun2(Red)

        type MyColors = TrafficColors

        val aaa:MyColors = Green

        println(aaa)
    }
}

