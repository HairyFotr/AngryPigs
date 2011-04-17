
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

(defn give-me-tree [node depth]
  (plane node))

(println (give-me-tree [1 2 3 5] 0))