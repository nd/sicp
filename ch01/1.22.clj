(defn divides? [a b]
  (= (rem b a) 0))

(defn find-divisor [n test-divisor]
  (cond (> (* test-divisor test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (recur n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn report-prime [n elapsed-time]
  (println n " *** " elapsed-time)
  true)

(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime n (- (System/currentTimeMillis) start-time))
    false))

(defn timed-prime-test [n]
  (start-prime-test n (System/currentTimeMillis)))

(defn search-for-primes [from to]
  (defn iter [i]
    (timed-prime-test i)
    (if (< i to)
      (recur (+ i 2))))
  (if (odd? from)
    (iter from)
    (iter (inc from))))

(defn find-n-primes-from [n from]
  (defn iter [i count]
    (if (timed-prime-test i)
      (if (< (inc count) n)
        (recur (+ i 2) (inc count)))
      (recur (+ i 2) count)))
  (if (odd? from)
    (iter from 0)
    (iter (inc from) 0)))

;user> (time (find-n-primes-from 3 1000))
;1009  ***  0
;1013  ***  0
;1019  ***  0
;"Elapsed time: 5.035836 msecs"
;nil
;user> (time (find-n-primes-from 3 10000))
;10007  ***  0
;10009  ***  1
;10037  ***  0
;"Elapsed time: 6.763849 msecs"
;nil
;user> (time (find-n-primes-from 3 100000))
;100003  ***  1
;100019  ***  4
;100043  ***  1
;"Elapsed time: 22.434489 msecs"
;nil
;user> (time (find-n-primes-from 3 1000000))
;1000003  ***  4
;1000033  ***  4
;1000037  ***  4
;"Elapsed time: 17.52695 msecs"
;nil

