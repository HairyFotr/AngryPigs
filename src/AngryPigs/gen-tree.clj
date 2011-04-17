
(ns AngryPig.gen-tree)

; the aim of this thing is to generate a tree
; main program expects a list of point,vector tuples that represent cyllindres making up the tree

; operative keyword: fractals

; data structure:
; node: (x y z d)  (vector with distance)
; tree: (node
;        (node
; 	(node
; 	 (node) (node) (node) (node) (node))
; 	(node
; 	 (node) (node) (node) (node) (node))
; 	(node
; 	 (node) (node) (node) (node) (node)))
;        (node
; 	(node
; 	 (node) (node) (node) (node) (node))
; 	(node
; 	 (node) (node) (node) (node) (node))
; 	(node
; 	 (node) (node) (node) (node) (node))))


(def primes [2 3 5 7 11 13 17 23 29])
(def max-depth 9)

; generate a tree from starting position v
(defn travel [node depth]
  (let [d (last node)]
    (concat (map #(+ %1 d) (butlast node)) [(/ d (nth primes depth))])))

(defn plane [N]
  (let [point (butlast (travel N 0))]
    (concat (butlast N)
	    [(apply + (map #(* (nth N %1) (nth point %1)) [0 1 2]))])))

(defn point-on-plane [plane]
  (let [d (last plane)]
    (let [plane (reverse (sort (butlast plane)))]
      (loop [point [] i 0 dd d]
	(if (>= i (count plane)) point
	    (let [component (/ dd (nth plane i))]
	      (recur (cons component point) (inc i) (- dd component))))))))

(defn vector-between-points [a b]
  (map #(- (nth b %1) (nth a %1)) [0 1 2]))

(defn length [v]
  (Math/sqrt (apply + (map #(* %1 %1) v))))

(defn normalize [v]
  (let [l (length v)]
    (map #(/ %1 l) v)))
  
(defn give-me-tree [node depth]
  (normalize (vector-between-points
	      (butlast (travel node 0))
	      (point-on-plane (plane node)))))

(println (give-me-tree [1 2 3 5] 0))