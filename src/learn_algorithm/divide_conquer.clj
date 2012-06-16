(ns learn-algorithm.divide-conquer)

(defn conquer-old [coll1 coll2]
  (loop [c* 0 rs* [] c1* coll1 c2* coll2] 
    (cond
     (or (empty? c1*) (empty? c2*)) [c* (concat rs* c1* c2*)]
     (<= (first c1*) (first c2*))
     (recur c* (conj rs* (first c1*)) (rest c1*) c2* )
     :else (recur (+ c* (count c1*))
                  (conj rs* (first c2*)) c1* (rest c2*)))))

(defn conquer [coll1 coll2]
  (let [c1 (count coll1)
        c2 (count coll2)]
    (loop [c* 0 rs* [] i 0 j 0]
      (cond
       (>= i c1) [c* (concat rs* (drop j coll2))]
       (>= j c2) [c* (concat rs* (drop i coll1))]
       (<= (nth coll1 i) (nth coll2 j))
       (recur c* (conj rs* (nth coll1 i)) (inc i) j)
       :else (recur (+ c* (- c1 i)) (conj rs* (nth coll2 j))
                    i (inc j))))))

(defn caculate [coll]
  (let [c (count coll)]
    (if (== c 1)
      [0 coll]
      (let [n (/ c 2)
            [n1 c1] (caculate (take n coll))
            [n2 c2] (caculate (drop n coll))
            [t sc] (conquer (vec c1) (vec c2))]
        [(+ n1 n2 t) sc]))))

(defn caculate-inversion [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (->> (line-seq rdr) 
         (map #(Integer/parseInt %))
         (caculate)
         (first))))
