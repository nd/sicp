(defn pascal [row col]
  (if (or (= col 1) (= col row)) 1
      (+ (pascal (dec row) (dec col))
         (pascal (dec row) col))))
      