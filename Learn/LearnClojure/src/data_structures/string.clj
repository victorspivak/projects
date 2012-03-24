(ns data_structures.string)

(println (str "I have " 5 " books.")) ;concat strings
(def text1 "Hello World")
(println (subs text1 6) (subs text1 0 5))
(println "Is " text1 " string? "(string? text1))

;regular expressions
(println "\n****************************************************************************")
(def pattern1 (re-pattern "[a-zA-Z]*"))
(def pattern2 #"[a-zA-Z]*")    ;another way to define pattern

(println (re-matches pattern1 "test") "  " (re-matches pattern1 "test1234"))

(def my-matcher (re-matcher #"[a-zA-Z]*" "test"))

;re-find is stateful
(println (re-find my-matcher))
(println (re-find my-matcher))
(println (re-find my-matcher))


(def my-matcher1 (re-matcher #"(\d\d\d)-(\d\d\d)-(\d\d\d\d)" "650-525-9568"))
(re-find my-matcher1 )
(println (re-groups my-matcher1 ))

(println (re-seq #"[A-Za-z]*" "Hello World")) ;re-seq returns lazy seq of matches







