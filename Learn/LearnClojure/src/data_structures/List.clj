(ns data_structures.List)

(println "------> List")
(def list1 (list 1 2 3 4 5))
(def list2 '(1 2 3 4 5))
(def list3 '(6 7 8 9))

(println "Is list1 list? " (list? list1))
(println list1 list2 "Count: " (count list1) (list? list1))
(println "Peek: " (peek list1) "   Pop: " (pop list1))
(println (cons 0 list2))
(println (cons list1 list2))
(println "Remove all elements that are > 3 " (remove #(> % 3) list1))
(println (into list2 list3 )) ; combines two lists

(println "Check for existance: " (some #(= % 3) list1))
(println "Contains?" (contains? (set list1) 3))    ;it coverts list to set and calls contains


