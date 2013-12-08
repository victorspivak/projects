object Sets {
    def main(args:Array[String]) {
        var jetSet = Set("Boeing", "Airbus")
        jetSet += "Lear"

        Console println "contains: " + jetSet.contains("Cessna") + "  " + jetSet.contains("Boeing")
        Console println "entire set: " + jetSet

        import scala.collection.immutable.HashSet
        val hashSet = HashSet("Tomatoes", "Chilies")
        println("Immutable hashSet: " + (hashSet + "Coriander"))
    }
}





