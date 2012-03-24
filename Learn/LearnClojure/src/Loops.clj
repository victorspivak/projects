(loop [i 0]
  (when (< i 5)
    (print "i = ")
    (println i)
    (recur (inc i))))

(println "----------------------------------------")

(dorun (for [i (range 0 5)]
         (println i)))

(println "----------------------------------------")

(doseq [i (range 0 5)]
  (println i))




