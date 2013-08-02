; to get class
(println (.getClass "Hello"))
(println "5 is Integer "(instance? Integer 5) "\nHello is Integer "(instance? Integer "Hello") "\nHello is String "(instance? String "Hello"))


;timing
(println (time (+ 2 5)))
