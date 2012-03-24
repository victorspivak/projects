(println "------> Collections")

(def list1 (list 1 2 3 4 5))
(def list2 '(1 2 3 4 5))
(def list3 '(6 7 8 9))
(def v3 (vec '(1 2 3 4 5 6 7 8 9)))  ;vec allows construct vector from a collection
(def v4 (conj v3 11 22 33))  ;conj appends elements to the vector

(println list1 list2 "Count: " (count list1) (list? list1))
(println "First: " (first list1) "Second: " (second list1) "4th: " (nth list1 4) "   Rest: " (rest list1))
(println "Next: " (next list1) " ButLast: " (butlast list1) "  Drop 3 last" (drop-last 3 list1))
(println "Take out first 2 elements " (nthnext list1 2))

(println (reverse list1))
(println (apply + list1))

(println (map #(+ % 3) list1)) ; adds 3 to every element
(println (map + list1 list2 v4))
(println (filter #(= (rem % 2) 1) list1))
(println (every? #(> % 0) list1) (every? #(> % 3) list1))
(println (not-every? #(> % 0) list1) (not-every? #(> % 3) list1))
(println (some #(> % 3) list1) (not-any? #(> % 3) list1))
