(ns learn-algorithm.divide-conquer)

(defn conquer-very-slow [coll1 coll2]
  (loop [c* 0 rs* [] c1* coll1 c2* coll2] 
    (cond
     (or (empty? c1*) (empty? c2*)) [c* (concat rs* c1* c2*)]
     (<= (first c1*) (first c2*))
     (recur c* (conj rs* (first c1*)) (rest c1*) c2* )
     :else (recur (+ c* (count c1*))
                  (conj rs* (first c2*)) c1* (rest c2*)))))

(defn conquer-slow [coll1 coll2]
  (let [c1 (count coll1)
        c2 (count coll2)]
    (loop [c* 0 rs* [] i 0 j 0]
      (cond
       (>= i c1) [c* (apply conj rs* (drop j coll2))]
       (>= j c2) [c* (apply conj rs* (drop i coll1))]
       (<= (nth coll1 i) (nth coll2 j))
       (recur c* (conj rs* (nth coll1 i)) (inc i) j)
       :else (recur (+ c* (- c1 i)) (conj rs* (nth coll2 j))
                    i (inc j))))))

(defn conquer [^ints rs ^ints coll st1 nt1 st2 nt2]
  (loop [t st1 s1* st1 n1* nt1 s2* st2 n2* nt2 c 0]
    (cond
     (== n1* 0) (do (System/arraycopy coll s2* rs t n2*) c) 
     (== n2* 0) (do (System/arraycopy coll s1* rs t n1*) c)
     (<= (aget coll s1*) (aget coll s2*))
     (do (aset rs t (aget coll s1*))
         (recur (inc t) (inc s1*) (dec n1*) s2* n2* c) )
     :else (do (aset rs t (aget coll s2*))
               (recur (inc t) s1* n1* (inc s2*)
                      (dec n2*) (+ c n1*))))))

(defn caculate [^ints rs ^ints coll s n]
  (if (== n 1)
    (do (aset rs s (aget coll s))
        0)
    (let [p (quot n 2)
          n1 (caculate rs coll s p)
          n2 (caculate rs coll (+ s p) (- n p))
          _ (System/arraycopy rs s coll s n)
          t  (conquer rs coll s p (+ s p) (- n p))]
      (+ n1 n2 t))))

(defn caculate-slow [coll]
  (let [c (count coll)]
    (if (== c 1)
      [0 coll]
      (let [n (/ c 2)
            [n1 c1] (caculate-slow (take n coll))
            [n2 c2] (caculate-slow (drop n coll))
            [t sc] (conquer-slow c1 c2)]
        [(+ n1 n2 t) sc]))))

(defn caculate-very-slow [coll]
  (let [c (count coll)]
    (if (== c 1)
      [0 coll]
      (let [n (/ c 2)
            [n1 c1] (caculate-very-slow (take n coll))
            [n2 c2] (caculate-very-slow (drop n coll))
            [t sc] (conquer-very-slow c1 c2)]
        [(+ n1 n2 t) sc]))))

(defn caculate-inversion-slow [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (->> (line-seq rdr) 
         (map #(Integer/parseInt %))
         (caculate-slow)
         (first))))

(defn caculate-inversion-very-slow [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (->> (line-seq rdr) 
         (map #(Integer/parseInt %))
         (caculate-very-slow)
         (first))))

(defn caculate-inversion [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (let [coll (->> (line-seq rdr)
                  (map #(Integer/parseInt %))
                  int-array)
          rs (int-array coll)]
      (caculate rs coll 0 (count coll)))))
