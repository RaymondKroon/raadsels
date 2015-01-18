(ns raadsels.bucket
  (:refer-clojure)
  (:require [ clojure.string :as str]))

(comment  (def buckets {:a (Bucket. 12 12) :b (Bucket. 8 0) :c (Bucket. 5 0)})
          (def target [6 6 0])
          (def p ( Path. buckets [])))

(defprotocol Pour
  (make-moves [this])
  (get-volumes [this]))

(defrecord Bucket [capacity volume])
(defrecord Move [from to]
  Object
  (toString [_] (str/join [from " -> " to])))

(declare pour)
(declare possible-moves)

(defrecord Path [buckets moves]
  Pour
  (make-moves [this]
    (for [move (possible-moves buckets)]
     (Path.  (pour buckets move) (conj moves move))))
  (get-volumes [this]
    (frequencies (map :volume (vals buckets)))))

(extend-type clojure.lang.PersistentVector
  Pour
  (make-moves [this]
    (mapcat make-moves this))
  (get-volumes [this]
    (map get-volumes this)))

(extend-type clojure.lang.LazySeq
  Pour
  (make-moves [this]
    (mapcat make-moves this))
  (get-volumes [this]
    (map get-volumes this)))

(defn possible-moves [buckets]
  (for [from (keys buckets)
        to (keys buckets)
        :when (not= from to)
        :when (not= (:volume (to buckets)) (:capacity (to buckets)))]
    (Move. from to )))

(defn pour [state move]
  (let [from ((:from move) state)
        to ((:to move) state)
        pour (min (:volume from) (- (:capacity to) (:volume to)))]
    (-> state (assoc-in [(:from move) :volume] (- (:volume from) pour))
        (assoc-in [(:to move) :volume] (+ (:volume to) pour)))
))

(defn give-solution [paths target]
  (for [path paths
        :when (= (get-volumes path) target)]
    path))

(defn solve
  ([startbuckets target]
   (solve [(Path. startbuckets [])] (frequencies target) #{startbuckets}))

  ([paths target explored]
   (println (count paths))
   (if (contains? (set (map get-volumes paths)) target)
     (give-solution paths target)
     (let [newpaths (make-moves paths)
           newstates (map :buckets newpaths)
           newexplored (conj explored newstates)]
     (recur newpaths target newexplored))
     ))
  )