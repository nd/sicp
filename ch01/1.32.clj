(defn accumulate-i [combiner null-value term a next b]
  (loop [result null-value, i a]
    (if (> i b)
      result
      (recur (combiner result (term i)) (next i)))))

(defn sum [term a next b]
  (accumulate-i + 0 term a next b))

(defn accumulate-r [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate-r combiner null-value term (next a) next b))))

(defn product [term a next b]
  (accumulate-r * 1 term a next b))