(defn fast-expt [b n]
  (defn iter [new-b new-n a]
    (if (= new-n 1)
      (* new-b a)
      (recur (* new-b new-b)
             (quot new-n 2)
             (* a (if (even? new-n)
                    1
                    new-b)))))
  (iter b n 1))