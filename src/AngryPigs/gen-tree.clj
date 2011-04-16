
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


; generate a tree from starting position v
(defn give-me-tree [point l]
  (map #(+ %1 l) point))

(println (give-me-tree [1 1 1] 5))