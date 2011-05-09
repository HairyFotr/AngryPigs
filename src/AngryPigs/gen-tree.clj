(ns AngryPigs.gen-tree)

(require 'geometry)


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
(def lengths [0.8 0.7 0.5 0.3 0.099 0.097 0.093 0.088 0.081])
(def max-depth 3)


(defn make-node [v l]
  (apply list (concat v [(float l)])))

(defn perpendicular-vector [v]
  (let [perp (reverse (sort v))]
    (normalize (cross-product v
                              (cons (* 0.5 (first perp))
                                    (rest perp))))))

(defn give-me-tree
  ([a b c d] (give-me-tree (make-node [a b c] d)
                           d
                           0))
  ([node baselen depth]
     (if (> depth max-depth) node

         (let [d (* baselen (nth lengths depth))]
           (let [first-branch (perpendicular-vector (butlast node))]

             (defn make-branches []
               (let [angle (/ (* 2 Math/PI) (nth primes depth))]
                 (loop [branches []
                        n (dec (nth primes depth))]
                   (if (<= n 0) (cons first-branch branches)
                       (recur (cons (normalize (rotate first-branch
                                                       (normalize (butlast node))
                                                       (* angle n)))
                                    branches)
                              (dec n))))))

             (defn curve-up [branches]
               (let [up-angle (/ Math/PI (+ 2 (rand-int 3)))]
                 (map #(normalize (rotate %1
					  (cross-product %1
							 (normalize (butlast node)))
					  up-angle))
                      branches)))



             (concat [node]
                     [(map #(give-me-tree
                             (make-node %1 d)
                             baselen
                             (inc depth))
                           (curve-up (make-branches)))]))))))

;(println (give-me-tree 0 2 0 5))
