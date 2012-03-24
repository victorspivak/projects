/*
 * User: Victor    Date: 1/20/12   Time: 10:56 PM
 */

var jetSet = Set("Boeing", "Airbus")
jetSet += "Lear"

Console println "contains: " + jetSet.contains("Cessna") + "  " + jetSet.contains("Boeing")
Console println "entire set: " + jetSet

import scala.collection.immutable.HashSet
val hashSet = HashSet("Tomatoes", "Chilies")
println("Immutable hashSet: " + (hashSet + "Coriander"))




