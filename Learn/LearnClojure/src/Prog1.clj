(def msg "Hello Clojure")
(println msg)

(def sum #(+ %1 %2))
(println (sum 2 3))

(defn add-up
  "Add all numbers below a given limit"
  ([limit](add-up limit 0 0))
  ([limit current sum]
    (if (< limit current)
      sum
      (recur limit (+ 1 current) (+ current sum)))))

(println (add-up 100000))

(loop [i 0]
  (if (= i 10)
    (println "end of the loop")
    (do (recur (+ i 1)))))

(defn fact1
  "Calculate factorial"
  ([n]
    (if (< n 1)
      1
      (* n (fact1 (- n 1))))))

(defn fact
  "Calculate factorial with recur"
  ([n] (fact n 1))
  ([n r]
    (if (< n 1)
      r
      (recur (- n 1) (* n r)))))


(time (println (fact1 2222)))
(time (println (fact 2222)))

(println (. "Hello" (toLowerCase) ))

(println (new String "Hello"))

(import '(java.util ArrayList))
(def v (new ArrayList))
(. v add 5)
(. v add 10)
(println v)