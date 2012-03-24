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

(defn calc
  ([params] (calc (first params) (subvec params 1)))
  ([res params] (if (empty? params)
                  res
                  (do
                    (calc ((first params) res (second params))
                      (subvec params 2))
                  )
                )
  )
)

(println "Result: " (calc [5 + 3 * 2 - 6 / 2]))
