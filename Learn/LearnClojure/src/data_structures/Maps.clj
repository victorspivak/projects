(ns data_structures.Maps)

(println "------> Map")
(def m1 {"a" 1 "b" 2 "c" 3})
(def m2 {:key1 1 :key2 2 :key3 3})
(def k (keyword "key3"))
(println m1 "  get: " (get m1 "c") "  short syntax " (m1 "c"))
(println (get m2 :key3) "   " (get m2 k) "   "  (m2 k) "   "  (k m2))

(defn build-large-map
  ([n] (build-large-map n {}))
  ([n map]
    (if (= n 0)
      map
      (recur (- n 1) (assoc map n (* 2 n)))
      )
    )
  )

(println (hash-map 1 2, 5 10, 20 40))
(println (sorted-map 1 2, 5 10, 20 40))

(def large-map (build-large-map 100))
(println (.getClass large-map ) large-map )

(defn build-large-smap
  "It builds sorted map"
  ([n] (build-large-smap n (sorted-map )))
  ([n map]
    (if (= n 0)
      map
      (recur (- n 1) (assoc map n (* 2 n)))
      )
    )
  )

(def large-smap (build-large-smap 100))
(println (.getClass large-smap ) large-smap )
(println ((build-large-smap 100) 50))

(defn lazy-counter [base increment]
  (lazy-seq
    (cons base (lazy-counter (+ base increment) increment))
  )
)

(println (take 10 (lazy-counter 0 2)))
(println (nth (lazy-counter 2 3) 1000000))

(def integers (iterate inc 0))
(println (nth integers 1000000))   ;this version takes memory
(println (nth (iterate inc 0) 1000000))




