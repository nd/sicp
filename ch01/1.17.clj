(ns ex.1.17
  (:refer-clojure :exclude [* double]))

(defn double [n]
  (+ n n))

(defn halve [n]
  (quot n 2))

(defn * [a b]
  (defn iter [new-a new-b i]
    (if (= new-b 1)
      (+ new-a i)
      (recur (double new-a)
             (halve  new-b)
             (if (even? new-b)
               i
               (+ i new-a)))))
  (iter a b 0))

(assert (= 4 (* 2 2)))
