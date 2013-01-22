object Maps {
    def main(args:Array[String]) {
        var treasureMap = Map[Int, String]()
        treasureMap += (1 -> "Go to island.")
        treasureMap += (2 -> "Find big X on ground.")
        treasureMap += (3 -> "Dig.")
        Console println treasureMap(2)

        val romanNumeral = Map(1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V")
        Console println romanNumeral
    }
}



