(ns raadsels.spiral)

(defn matrix [n]
  (into [] (for [rownum (range n)
                 :let [start (-> rownum (* n) (+ 1))
                       stop (+ start n)
                       row (into [] (range start stop)) ]]
             row
             )))

(defn transpose [matrix]
  (if (empty? matrix) matrix
      (apply mapv vector matrix)))

(defn horizontal-flip [matrix]
  (map reverse matrix))

(defn rotate-ccw [matrix]
  (-> matrix horizontal-flip transpose))

(defn- spiral [matrix]
  (loop [matrix matrix
         result []]
    (if (empty? matrix)
      result
      (recur (-> matrix rest rotate-ccw) (concat result (first matrix))))
    ))

(defn spiral-cw [n]
  (spiral (matrix n)))

(defn spiral-ccw [n]
  (spiral (-> n matrix transpose)))
