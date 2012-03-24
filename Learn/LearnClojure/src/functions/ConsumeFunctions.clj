(ns functions.ConsumeFunctions)

(defn arg-switch
  "Applies the supplied function to the arguments in both possible orders. "
  [fun arg1 arg2]
  (list (fun arg1 arg2) (fun arg2 arg1)))

(println (arg-switch / 2 3))
(println (arg-switch str "Hello " "World "))
(println (arg-switch (fn [a b] (/ a (* b b))) 2 3))

(defn rangechecker
  "Returns a function that determines if a number is in a provided range."
  [min max]
  (fn [num]
    (and (<= num max)
      (<= min num))))

(def myrange (rangechecker 5 10))
(println (myrange 7) (myrange 17))

; Using partial to Curry Functions
(println "===========================================")
(println "Using partial to Curry Functions")

(def times-pi (partial * 3.14159))
(println (times-pi 2))

(println (map #(* 0.01 %1) [5000 100 50]))
(println (map (partial * 0.01) [5000 100 50]))






