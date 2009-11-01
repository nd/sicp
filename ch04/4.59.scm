;;a
(совещание ?somewhere (пятница ?time))

;;b
(rule (время-совещания ?person ?day-and-time)
      (and (должность ?person (?department . ?any))
           (or (совещание ?department ?day-and-time)
               (совещание компания ?day-and-time))))

;;c
(время-совещания (Хакер Лиза П) (среда ?any-time))