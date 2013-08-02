(println (new java.util.Date))
(println (. (new java.util.HashMap) (containsKey "key")))
(println (. Integer MAX_VALUE) "   "  (. Character TYPE))

(import '(java.util ArrayList))
(import '(java.io File) '(java.util HashMap))
(def v (new ArrayList))
(. v add 5)
(. v add 10)
(println v)



