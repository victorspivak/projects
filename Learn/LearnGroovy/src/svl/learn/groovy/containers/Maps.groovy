package svl.learn.groovy.containers

scores = [ "Brett":100, "Pete":"Did not finish", "Andrew":86.87934 ]
println "Map size: ${scores.size()}"

println scores["Pete"]
println scores.Pete

scores["Pete"] = 3
println scores.Pete

emptyMap = [:]
println emptyMap.size()
