(defn make-point [x y]
  [x y])

(defn x-point [point]
  (first point))

(defn y-point [point]
  (second point))

(defn print-point [point]
  (do 
    (println "(" (x-point point) ", " (y-point point) ")")))

(defn make-segment [start end]
  [start end])

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

(defn midpoint-segment [segment]
  (make-point 
   (/ (+ (x-point (start-segment segment)) 
         (x-point (end-segment segment))) 
      2.0)
   (/ (+ (y-point (start-segment segment))
         (y-point (end-segment segment)))
      2.0)))