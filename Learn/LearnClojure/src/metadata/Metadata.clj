(ns metadata.Metadata)

(def v1 (with-meta [1 2] {:about "It is a small vector" :author "vspivak"}))
(println (meta v1))
(println (:about (meta v1)) )

