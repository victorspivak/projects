; Anonymous functions
(println ((fn [x] (+ x 1)) 9))

(def my-inc1 (fn [x] (+ x 1)))

(defn my-inc2
  "Incrementing value by 1"
  [x] (+ x 1)
)

;Another way to document a function
(defn my-inc3
  {:doc "Incrementing value by 1"}
  [x] (+ x 1)
  )

(println "*************************************************")
(println (doc my-inc1))

(println "*************************************************")
(println (doc my-inc2))

(println "*************************************************")
(println (doc my-inc3))

(println "*************************************************")
(println (my-inc3 (my-inc2 (my-inc1 9))))




