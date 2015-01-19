(ns raadsels.bucket
  (:refer-clojure)
  (:require [ clojure.string :as str])
  (:require [criterium.core :refer :all]))

(comment  (def buckets {:a (Bucket. 12 12) :b (Bucket. 8 0) :c (Bucket. 5 0)})
          (def target [6 6 0])
          (def p ( Path. buckets [])))

(defprotocol Pour
  (make-moves [this not-wanted-results])
  (get-volumes [this]))

(defrecord Bucket [capacity volume])
(defrecord Move [from to]
  Object
  (toString [_] (str/join [from " -> " to])))

(declare pour)
(declare possible-moves)

(defrecord Path [buckets moves]
  Pour
  (make-moves [this not-wanted-results]
    (for [move (possible-moves buckets)
          :let [new-state (pour buckets move)]
          :when (not (contains? not-wanted-results new-state))]
        (Path.  new-state (conj moves move))))
  (get-volumes [this]
    (into [] (map :volume (vals buckets)))))

(defn possible-moves [buckets]
  (into [] (for [from (keys buckets)
          to (keys buckets)
          :when (not= from to)
          :when (not= (:volume (to buckets)) (:capacity (to buckets)))]
      (Move. from to ))))

(defn pour [state move]
  (let [from ((:from move) state)
        to ((:to move) state)
        pour (min (:volume from) (- (:capacity to) (:volume to)))]
    (-> state (assoc-in [(:from move) :volume] (- (:volume from) pour))
        (assoc-in [(:to move) :volume] (+ (:volume to) pour)))
))

(defn find-solution [paths targets]
  (first (drop-while #(not (contains? targets (get-volumes %))) paths)))

(defn solve
  ([startbuckets target]
   (solve [(Path. startbuckets [])] target #{startbuckets}))

  ([paths targets explored]
   ;;(println (count paths))
   (if-let [solution ( find-solution paths targets)]
     solution
     (let [newpaths (mapcat #(make-moves % explored) paths)
           newstates (map :buckets newpaths)
           newexplored (conj explored newstates)]
     (recur newpaths targets newexplored))
     ))
  )

;;(time (solve {:a (Bucket. 12 12) :b (Bucket. 8 0) :c (Bucket. 5 0)}  #{[6 6 0] [6 0 6] [0 6 6]}))
(with-progress-reporting (bench (solve {:a (Bucket. 12 12) :b (Bucket. 8 0) :c (Bucket. 5 0)} #{[6 6 0] [6 0 6] [0 6 6]}) :verbose))
