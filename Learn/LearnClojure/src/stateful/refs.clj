(ns stateful.refs)

(def ref1 (ref 5))
(println ref1 " --> " (deref ref1))
(println ref1 " --> " @ref1)

(println (+@ref1 10))
(dosync (ref-set ref1 10))
(println ref1 " --> " @ref1)

(dosync (alter ref1 + 10))
(println ref1 " --> " @ref1)

(dosync (commute ref1 + 3)) ;commute is used for Commutative functions
(println ref1 " --> " @ref1)

(println "============================================================")
(def account1 (ref 1000))
(def account2 (ref 1500))
(defn positive? [x] (>= x 0))

(defn my-watch [key identity old-val new-val]
  (println (str key " : " old-val " ==> " new-val)))

(add-watch account1 "Account1" my-watch )
(add-watch account2 "Account2" my-watch )

(set-validator! account1 positive?)
(set-validator! account2 positive?)
(defn transfer
  "transfers amount of money from a to b"
  [a b amount]
  (dosync
    (alter a - amount)
    (alter b + amount)))
(transfer account1 account2 300)
(transfer account2 account1 50)

(println "Account #1:" @account1)
(println "Account #2:" @account2)

(println "============================================================")

(try (transfer account2 account1 2000) (catch Exception e (prn "****** ERROR: " (. e getMessage))))

(println "Account #1:" @account1)
(println "Account #2:" @account2)

(println "============================================================")

(def my-contacts (ref []))
(defn add-contact
  "adds a contact to the provided contact list"
  [contacts contact]
  (dosync
    (alter contacts conj (ref contact))))
(defn print-contacts
  "prints a list of contacts"
  [contacts]
  (doseq [c @contacts]
    (println (str "Name: " (@c :lname) ", " (@c :fname)))
    ))
(add-contact my-contacts {:fname "Luke" :lname "VanderHart"})
(add-contact my-contacts {:fname "Stuart" :lname "Sierra"})
(add-contact my-contacts {:fname "John" :lname "Doe"})
(print-contacts my-contacts)

(println "============================================================")
(println "Adding Initials to the Address Book")

(defn add-initials
  "adds initials to a single contact and returns it"
  [contact]
  (assoc contact :initials
    (str (first (contact :fname)) (first (contact :lname)))))
(defn add-all-initials
  "adds initials to each of the contacts in a list of contacts"
  [contacts]
  (dosync
    (doseq [contact (ensure contacts)]
      (alter contact add-initials))))
(defn print-contacts-and-initials
  "prints a list of contacts, with initials"
  [contacts]
  (dorun (map (fn [c]
                (println (str "Name: " (@c :lname) ", " (@c :fname) " (" (@c
                                                                           :initials) ")")))
           @contacts)))
(defn print-contacts-and-initials
  "prints a list of contacts, with initials"
  [contacts]
  (doseq [c @contacts]
    (println (str "Name: " (@c :lname) ", " (@c :fname) " (" (@c :initials) ")"))))
(add-all-initials my-contacts)
(print-contacts-and-initials my-contacts)





