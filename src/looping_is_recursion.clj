(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e]
                 (if (zero? e)
                   acc
                   (recur (* acc b) b (dec e))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [result s]
                 (if (empty? s)
                   result
                   (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc s1 s2]
                 (if (and (empty? s1)
                          (empty? s2))
                   acc
                   (recur (and acc
                               (= (first s1) (first s2)))
                          (rest s1)
                          (rest s2))))]
    (if-not (= (count seq1) (count seq2))
      false
      (helper true seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         s a-seq]
    (if (empty? s)
      nil
      (if (true? (pred (first s)))
        acc
        (recur (inc acc) (rest s))))))

(defn avg [a-seq]
  (loop [acc 0
         s a-seq]
    (if (empty? s)
      (/ acc (count a-seq))
      (recur (+ acc (first s)) (rest s)))))

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  (memoize
   (defn fib [f1 f2 x]
     (if (zero? x)
       f2
       (fib f2 (+ f1 f2) (dec x)))))
  (fib 1 0 n))

(defn cut-at-repetition [a-seq]
  (loop [seq-1 []
         seq-2 a-seq]
    (cond (empty? seq-2) seq-1
          (some #(= (first seq-2) %) seq-1) seq-1
          :else (recur (conj seq-1 (first seq-2)) (rest seq-2)))))
