(ns AngryPigs.helloworld)
    
(defn neki [a]
  (+ a 2))

(defn length [v]
  (Math/sqrt (apply + (map #(* %1 %1) v))))

(println "this is a lol")
