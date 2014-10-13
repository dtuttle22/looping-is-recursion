(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (== n exp)
                   acc
                   (recur (* acc base) (inc n))))]

        (if (zero? exp)
          1
          (helper 1 0))))

(defn last-element [a-seq]
  (let [helper (fn [x] 
                 (if (empty? (rest x))
                   (first x)
                   (recur (rest x))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [x y]
                 (cond 
                   (not= (count x) (count y)) false
                   (and (empty? x) (empty? y)) true
                   (not= (first x) (first y)) false
                   :else (recur (rest x) (rest y))))]

    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0 
         the-seq a-seq]
    (cond
      (empty? the-seq) nil
      (pred (first the-seq)) index
      :else (recur (inc index) (rest the-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         size (count a-seq)
         the-seq a-seq]
    (if (empty? the-seq)
      (/ acc size)
      (recur (+ acc (first the-seq)) size (rest the-seq)))))

(defn toggle [a-set elem]
  ((if (contains? a-set elem) disj conj) a-set elem))

(defn parity [a-seq]
  (loop [a-set #{}
         the-seq a-seq]
    (if (empty? the-seq)
      a-set
      (recur (toggle a-set (first the-seq))
             (rest the-seq)))))

(defn fast-fibo [n]
  (loop [fn-1 0
         fn 1
         acc 1]
    (cond 
      (zero? n) 0
      (== acc n) fn
      :else (recur fn (+ fn-1 fn) (inc acc)))))

(defn in? [seq elem]
  "true if seq contains elem, false otherwise"
  (boolean (some #(= % elem) seq)))

(defn cut-at-repetition [a-seq]
  (loop [a-vec []
         the-seq a-seq]
    (cond
      (empty? the-seq) a-vec
      (in? a-vec (first the-seq))  a-vec
      :else (recur (conj a-vec (first the-seq))
                   (rest the-seq)))))
