(ns learn-algorithm.quick-sort)

(defn get-first-pivot [^ints coll start end]
  start)

(defn get-last-pivot [^ints coll start end]
  end)

(defn get-optimize-pivot [^ints coll start end]
  (let [m (+ start (quot (- end start) 2))
        s-value (aget coll start)
        e-value (aget coll end)
        m-value (aget coll m)]
    (cond
     (or (> e-value m-value s-value)
         (> s-value m-value e-value)) m
     (or (> s-value e-value m-value)
        (> m-value e-value s-value)) end
     :else start)))

(defn swap [^ints array-coll i j]
  (if (== i j)
    array-coll
    (let [i-value (aget array-coll i)]
      (do (aset array-coll i (aget array-coll j))
          (aset array-coll j i-value)
          array-coll))))

(defn quick-sort [fn-get-pivot ^ints array-coll start end]
  (if (< (- end start) 1)
    0
    (let [s (fn-get-pivot array-coll start end)
          p (aget array-coll s)]
      (do 
        (swap array-coll s start)
        (loop [i (inc start) j (inc start)]
          (cond
           (== j (inc end)) (do
                        (swap array-coll (dec i) start)
                        (+ (- end start)
                           (quick-sort fn-get-pivot array-coll
                                       i end)
                           (quick-sort fn-get-pivot array-coll
                                       start (- i 2))))
           (<= (aget array-coll j) p) (do
                                        (swap array-coll i j)
                                        (recur (inc i) (inc j)))
           :else (recur i (inc j))))))))

(defn three-compute [v]
  (let [end (dec (count v))]
    (map #(quick-sort % (int-array v) 0 end)
         [get-first-pivot get-last-pivot get-optimize-pivot])))

(defn caculate-comparison [f fn-get-pivot]
  (with-open [rdr (clojure.java.io/reader f)]
    (let [coll (->> (line-seq rdr)
                  (map #(Integer/parseInt %)))]
      (three-compute coll))))



