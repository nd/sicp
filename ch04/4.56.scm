;;a
(and (начальник ?x (Бен Битобор))
     (адрес ?x . ?address))

;;b
(and (зарплата (Битобор Бен) ?bitobor-amount)
     (зарплата ?person ?amount)
     (lisp-value > ?bitobor-amount ?amount))

;;c
(and (начальник ?person ?boss)
     (not (должность ?boss (компьютеры . ?))))