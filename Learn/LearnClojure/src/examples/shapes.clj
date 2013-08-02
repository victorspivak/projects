(ns examples.shapes)

(defn move-to [shape x y] (merge shape {:x x :y y}))
(defn r-move-to [shape x y] (move-to shape (+ (shape :x) x) (+ (shape :y) y)))
(derive ::rectangle ::shape)
(derive ::circle ::shape)
(defn rectangle [x y w h] (with-meta {:x x :y y :width w :height h} {:type ::rectangle}))
(defn circle [x y r] (with-meta {:x x :y y :radius r} {:type ::circle}))
(defmulti draw (fn [shape] ((meta shape) :type)))
(defmethod draw ::rectangle [rect]
  (println (str "Draw a Rectangle at:(" (rect :x) ", " (rect :y)
             "), width " (rect :width) ", height " (rect :height))))
(defmethod draw ::circle [circle]
  (println (str "Draw a Cicrle at:(" (circle :x) ", " (circle :y)
             "), radius " (circle :radius))))

(def scribble [(rectangle 10 20 5 6) (circle 15 25 8)])

(doseq [shape scribble]
  (draw shape)
  (let [s (r-move-to shape 100 100)]
    (draw s)))

(draw (assoc (rectangle 0 0 15 15) :width 30))

