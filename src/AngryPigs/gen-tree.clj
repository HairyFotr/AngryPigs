(ns AngryPigs.gen-tree)

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
(def max-depth 2)
(def I [[1 0 0]
	[0 1 0]
	[0 0 1]])
(def nums (iterate inc 0))

; generate a tree from starting position v
(defn length [v]
  (Math/sqrt (apply + (map #(* %1 %1) v))))

(defn round [decimals n]
  (let [m (Math/pow 10 decimals)]
    (/ (Math/round (* n m)) m)))

(defn normalize [v]
  (let [l (length v)]
    (map #(round 4 (/ %1 l)) v)))

(defn make-node [v l]
  (apply list (concat v [(float l)])))


; taken from http://steve.hollasch.net/cgindex/math/rotvec.html
;    let
;        [v] = [vx, vy, vz]      the vector to be rotated.
;        [l] = [lx, ly, lz]      the vector about rotation
;              | 1  0  0|
;        [i] = | 0  1  0|           the identity matrix
;              | 0  0  1|
;
;              |   0  lz -ly |
;        [L] = | -lz   0  lx |
;              |  ly -lx   0 |
;
;        d = sqrt(lx*lx + ly*ly + lz*lz)
;        a                       the angle of rotation
;
;    then
;
;   matrix operations gives:
;
;    [v] = [v]x{[i] + sin(a)/d*[L] + ((1 - cos(a))/(d*d)*([L]x[L]))}
(defn dot-product [v1 v2]
  (apply + (map #(* (nth v1 %1) (nth v2 %1))
		(take (count v1) nums))))

; a × b = (a2b3 − a3b2) i + (a3b1 − a1b3) j + (a1b2 − a2b1) k = (a2b3 − a3b2, a3b1 − a1b3, a1b2 − a2b1).
(defn cross-product [a b]
  [(- (* (nth a 1) (nth b 2)) (* (nth a 2) (nth b 1)))
   (- (* (nth a 2) (nth b 0)) (* (nth a 0) (nth b 2)))
   (- (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0)))])

(defn row [m n]
  (nth m n))

(defn column [m n]
    (map #(nth %1 n) m))

(defn *matrices [m1 m2]
  (let [cols (count (first m2))]
    (defn *row [r]
      (map #(dot-product (row m1 r) (column m2 %1))
	   (take cols nums)))
    (map *row (take (count m1) nums))))

(defn +matrices [m1 m2]
  (let [cols (count (first m2))]
    (defn +row [r]
      (map #(+ (nth (row m1 r) %1) (nth (row m2 r) %1))
	   (take cols nums)))
    (map +row (take (count m1) nums))))

(defn *scalar [m s]
  (defn *row [r]
    (map #(* s %1) r))
  (map *row m))

(defn transpose [m]
  (map #(column m %1) (take (count (first m)) nums)))

;    [v] = [v]x{[i] + sin(a)/d*[L] + ((1 - cos(a))/(d*d)*([L]x[L]))}
(defn rotate [v axis angle]
  (let [ax (nth axis 0)
        ay (nth axis 1)
        az (nth axis 2)]
    (let [L [[0       az      (- ay)]
             [(- az)  0       ax]
             [ay      (- ax)  0 ]]]
      (let [d (length axis)]

        (first
         (*matrices [v]
                    (+matrices I
                               (+matrices
                                (*scalar L
                                         (/ (Math/sin angle)
                                            d))
                                (*scalar (*matrices L L)
                                         (/ (- 1 (Math/cos angle))
                                            (* d d)))))))))))


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

         (let [d (/ baselen (nth primes depth))]
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
               (let [up-angle (/ Math/PI 3)]
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

(println (give-me-tree 0 2 0 5))
