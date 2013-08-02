(ns stateful.Agents)

(def my-agent (agent 5))
(println my-agent " ===> " @my-agent )

(defn long-func [a b]
  (do
    (. Thread sleep 1000)
    (prn (. Thread currentThread))
    (+ a b)
  ))
(send-off my-agent long-func 10)
(println my-agent " ===> " @my-agent )
(send my-agent + 3)
(println my-agent " ===> " @my-agent )

(. Thread sleep 1000)
(println my-agent " ===> " @my-agent )


(println "=======================================================")
(println "Dealing with failed agents")


(def an-agent (agent 10))

(try (send an-agent / 0) (catch Exception e (prn "in catch")) (finally (prn "in finally")))
(try (send an-agent + 1) (catch Exception e (prn "in catch")) (finally (prn "in finally")))
(try (send an-agent + 1) (catch Exception e (prn "in catch")) (finally (prn "in finally")))

(println (agent-error an-agent))
(restart-agent an-agent 5 :clear-actions true)
(send an-agent + 1)
(await an-agent )
(println an-agent " ===> " @an-agent )

(send-off (agent 5) long-func 10)
(send-off (agent 5) long-func 10)
(send-off (agent 5) long-func 10)
(send-off (agent 5) long-func 10)
(send-off (agent 5) long-func 10)

(. Thread sleep 2000)

(shutdown-agents)

