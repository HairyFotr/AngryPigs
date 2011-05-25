(ns AngryPigs.gen-tree)

(require 'geometry)


; the aim of this thing is to generate a tree
; main program expects a list of point,vector tuples that represent cyllindres making up the tree

; operative keyword: fractals

; data structure:sort of
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

(defn random-up-angle [depth]
  (if (zero? depth)
    (/ Math/PI (rand-nth (list 3 3 4 4 5)))
    (/ Math/PI (rand-nth (list 2 2 3 3 4 4 4 5 5 5 6 7)))))

(defn angle-noise []
  (/ Math/PI (* (rand-nth (flatten (list (replicate 3 10)
                                         (replicate 4 11)
                                         (replicate 5 12)
                                         (replicate 6 13)
                                         (replicate 7 14))))
                (rand-nth (list -1 1 -1 1 -1 1)))))

(defn weighed-random-choice [depth lengths]
  (defn weight [x depth]
  ; cos(x + sin(x)*0.9)*0.5+0.5
    (let [x (* x (/ Math/PI (count lengths)))
          depth (* depth (/ Math/PI (count lengths)))]
      (int (Math/floor (* 10
                          (+ 0.5 (* 0.5
                                    (Math/cos (+ (- x depth)
                                                 (* 1.5 (Math/sin (- x depth))))))))))))

  (defn choices []
    (flatten (map #(replicate (weight %1 depth) %1)
                  (take (count lengths)
                        (iterate inc 0)))))

  (nth lengths
       (rand-nth (choices))))

(defn random-length [baselen depth]
  (* baselen (weighed-random-choice depth lengths)))

;(println (random-length 5 0))

(defn give-me-tree
  ([a b c d] (give-me-tree (make-node [a b c] d)
                           d
                           0))
  ([node baselen depth]
     (if (> depth max-depth) node

	 (let [first-branch (perpendicular-vector (butlast node))]

	   (defn make-branches []
	     (let [angle (/ (* 2 Math/PI) (nth primes depth))]
	       (loop [branches []
		      n (dec (nth primes depth))]
		 (if (<= n 0) (cons first-branch branches)
		     (recur (cons (normalize (rotate first-branch
						     (normalize (butlast node))
						     (+ (angle-noise) (* angle n))))
				  branches)
			    (dec n))))))

	   (defn curve-up [branches]
	     (map #(normalize (rotate %1
				      (cross-product %1
						     (normalize (butlast node)))
				      (random-up-angle depth)))
		  branches))



	   (concat [node]
		   [(map #(give-me-tree
			   (make-node %1
				      (random-length baselen depth))
			   baselen
			   (inc depth))
			 (curve-up (make-branches)))])))))

;(println (give-me-tree 0 2 0 5))

