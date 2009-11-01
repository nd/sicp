(rule (независим ?person)
      (and (должность ?person (?department . ?any))
           (not (имеет-начальника-в-отделе ?person ?department))))

(rule (имеет-начальника-в-отделе ?person ?department)
      (and (должность ?person (?department1 . ?any))
           (подчиняется ?person ?boss)
           (должность ?boss (?department1 . ?any))))