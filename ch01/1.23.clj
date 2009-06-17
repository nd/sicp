(ns gcd)

(load-file "1.22.clj")

(defn find-divisor [n test-divisor]
  (defn next-divisor [prev-divisor]
    (if (= prev-divisor 2)
      3
      (+ prev-divisor 2)))
  (cond (> (* test-divisor test-divisor) n) n
        (gcd/divides? test-divisor n) test-divisor
        :else (recur n (next-divisor test-divisor))))

