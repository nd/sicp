(defn filtered-accumulate [combiner null-value term a next b filter]
  (loop [result null-value, i a]
    (if (> i b)
      result
      (recur (combiner result 
                       (if (filter i)
                         (term i)
                         null-value))
             (next i)))))


(defn sum-prime-squares [a b]
  (defn square [x] (* x x))
  (filtered-accumulate + 0 square a inc b prime?))