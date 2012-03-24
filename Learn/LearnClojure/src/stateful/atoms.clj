(ns stateful.atoms)

(def my-atom (atom 5))
(println my-atom " ===> " @my-atom )
(swap! my-atom + 3)
(println my-atom " ===> " @my-atom )
(reset! my-atom 1)
(println my-atom " ===> " @my-atom )

