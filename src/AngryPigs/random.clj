
(def lengths [0.8 0.7 0.5 0.3 0.099 0.097 0.093 0.088 0.081])

(defn weighed [depth]
  (flatten (map #(replicate (- (count lengths)
                               (Math/abs (- %1 depth))) %1)
                (take (count lengths)
                      (iterate inc 0)))))

(defn random-length [baselen depth]
  (* baselen
     (nth lengths
          (rand-nth (weighed depth)))))

(println (weighed 0))
