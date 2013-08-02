; concatenation strings
(def msg (str "Hello," " world!"))
(println msg)

; if form
(defn is-pos? "Checks if number is positive"
  [n] (if (>= n 0)
    true
    false)
)
(println (is-pos? 5) "   " (is-pos? -5))

; do form

(defn is-pos-verbose? "Checks if number is positive"
  [n] (if (>= n 0)
        (do (println "GREAT!!!")
        true)
        (do (println "OOPS!!!")
        false)
        )
  )
(println (is-pos-verbose? 5) "   " (is-pos-verbose? -5))

;The let form
(println (let [x 2] (+ x 8)))
(println (let [x 2 y 8] (+ x y)))
(println (let [color "Red" phrase (str "Color is: " color)]
           (str "Clojure says: " phrase)))




