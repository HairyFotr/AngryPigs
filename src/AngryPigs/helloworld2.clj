(ns AngryPig.helloworld2
    (:gen-class
      ;:name AngryPig.helloworld2
      :state state
      :init init
      :methods [       
       [neki [Object Object] Object]
      ]))
    
(defn -init [] [[] (atom [])])

;(defn -neki [this]
;    (println "hello world"))
(defn -neki [a b]
    a)

