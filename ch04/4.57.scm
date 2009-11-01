(rule (может-заменить ?person1 ?person2)
      (and (должность ?person1 ?job1)
           (должность ?person2 ?job2)
           (or (same ?job1 ?job2)
               (может-замещать ?job1 ?job2))
           (not (same ?person1 ?person1))))

;;a
(может-заменить ?x (Фект Пабло Э))

;;b
(and (может-заменить ?person1 ?person2)
     (зарплата ?person1 ?amount1)
     (зарплата ?person2 ?amount2)
     (lisp-value > ?amount2 ?amount1))