(load-file "1.42.clj")

(defn repeated [f n]
  (loop [i 1, result f]
    (if (= i n)
      result
      (recur (inc i) (compose f result)))))