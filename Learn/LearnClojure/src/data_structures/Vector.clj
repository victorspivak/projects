(ns data_structures.Vector)

(println )
(println "------> Vector")
(def v1 [1 2 [10 20]])
(def v2 (vector 0 111 222 333))
(def v3 (vec '(1 2 3 4 5 6 7 8 9)))  ;vec allows construct vector from a collection
(def v4 (conj v3 11 22 33))  ;conj appends elements to the vector
(def v5 (assoc v2 1 1111)) ;assoc returns new list with replaced the specified element in the vector by new value
(println "Vector: " (get v1 2))
(println (get v2 2) (v2 1) (v2 2))
(println (get v1 3 -1)) ; get non existing element and return default value -1

(println v3 "Is v3 vector? "(vector? v3))
(println "peek takes the last vector element " (peek v3))
(println "pop returns vector without last element " (pop v3))
(println v4 (count v4))
(println v5 (count v5))
(println "subvec returns subvector staring from specified position to the end"(subvec v4 3))
(println "subvec returns subvector staring from specified position to the specified postion "(subvec v4 3 5))


