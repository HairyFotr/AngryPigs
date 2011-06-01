(ns AngryPigs.gen-tree)

(require 'geometry)


(def primes [2 3 5 7 11 13 17 23 29])
(def lengths [0.8 0.7 0.5 0.3 0.099 0.097 0.093 0.088 0.081])
(def max-depth 4)


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

(defn gravity-angle [depth]
  (/ Math/PI 5))

(defn weighed-random-choice [choices weight]
  (defn indexes []
    (flatten (map #(replicate (weight %1) %1)
                  (take (count choices)
                        (iterate inc 0)))))

  (nth choices
       (rand-nth (indexes))))

(defn random-length [baselen depth]
  (defn weight [x pivot]
    (if (< x pivot) 0
	(int (Math/floor (/ 9 (inc (* 3 (- x pivot))))))))

  (* baselen (weighed-random-choice lengths #(weight %1 depth))))

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

	   (defn gravity [branches]
	     (map #(normalize (rotate %1
				      (cross-product %1
						     (normalize (butlast node)))
				      1))
		  branches))

	   (concat [node]
		   [(map #(give-me-tree
			   (make-node %1
				      (random-length baselen depth))
			   baselen
			   (inc depth))
			 (gravity (curve-up (make-branches))))])))))

(println (give-me-tree 0 2 0 5))

