object Maps {
  def main(args: Array[String]) {
    var treasureMap = Map[Int, String]()
    treasureMap += (1 -> "Go to island.")
    treasureMap += (2 -> "Find big X on ground.")
    treasureMap += (3 -> "Dig.")
    Console println treasureMap(2)

    val romanNumeral = Map(1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V")
    Console println romanNumeral

    val transformed = treasureMap.foldLeft(List[String]()) { (l, e) => e._2 :: l}
    println(transformed)

    //Check immuttable map implementation
    val m1  = Map(1->"A")
    val m2  = m1 + (2->"B")
    val m3  = m2 + (3->"C")
    val m4  = m3 + (4->"D")
    val m5  = m4 + (5->"E")
    val m6  = m5 + (6->"F")
    val m7  = m6 + (7->"G")
    val m8  = m7 + (8->"H")
    val m9  = m8 + (9->"I")
    val m10 = m9 + (10->"J")

    val v = m3.get(4)
    println(v)

  }
}



