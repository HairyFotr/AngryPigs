
(def lengths [0.8 0.7 0.5 0.3 0.099 0.097 0.093 0.088 0.081])

(defn weighed [depth weight]
  (flatten (map #(replicate (weight %1 depth) %1)
                (take (count lengths)
                      (iterate inc 0)))))

(defn random-length [baselen depth]
  (* baselen
     (nth lengths
          (rand-nth (weighed depth)))))

(defn weight [x depth]
  (- (count lengths)
     (Math/abs (- x depth))))

(defn weight [x pivot]
  ; cos(x + sin(x)*0.9)*0.5+0.5
  (let [x (* x (/ Math/PI (count lengths)))
        pivot (* pivot (/ Math/PI (count lengths)))]
    (int (Math/floor (* 10
                        (+ 0.5 (* 0.5
                                  (Math/cos (+ (- x pivot)
                                               (* 0.9 (Math/sin (- x pivot))))))))))))

(defn weighed-random-choice [choices pivot weight]
  (defn indexes []
    (flatten (map #(replicate (weight %1 pivot) %1)
                  (take (count choices)
                        (iterate inc 0)))))

  (nth choices
       (rand-nth (indexes))))


(loop [i 0]
  (println (weighed-random-choice lengths i weight))
  (if (< i 8)
    (recur (inc i))))
