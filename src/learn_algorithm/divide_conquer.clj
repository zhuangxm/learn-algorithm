(ns learn-algorithm.divide-conquer)

(defn conquer [coll1 coll2]
  (loop [c* 0 rs* [] c1* coll1 c2* coll2] 
    (cond
     (or (empty? c1*) (empty? c2*)) [c* (concat rs* c1* c2*)]
     (<= (first c1*) (first c2*))
     (recur c* (conj rs* (first c1*)) (rest c1*) c2* )
     :else (recur (+ c* (count c1*))
                  (conj rs* (first c2*)) c1* (rest c2*)))))

(defn caculate [coll]
  (let [c (count coll)]
    (if (== c 1)
      [0 coll]
      (let [n (/ c 2)
            [n1 c1] (caculate (take n coll))
            [n2 c2] (caculate (drop n coll))
            [t sc] (conquer c1 c2)]
        [(+ n1 n2 t) sc]))))

(defn caculate-inversion [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (->> (line-seq rdr) 
         (map #(Integer/parseInt %))
         (caculate)
         (first))))

