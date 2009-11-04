(сын Adam Cain)
(сын Cain Enoch)
(сын Enoch Irad)
(сын Irad Mehujael)
(сын Mehujael Methushael)
(сын Methushael Lamech)
(сын Ada Jabal)
(сын Ada Jubal)
(жена Lamech Ada)


(rule (внук ?f ?g)
      (and (сын ?f ?s)
           (сын ?s ?g)))

(rule (сын-отца ?f ?s)
      (and (жена ?f ?w)
           (сын ?w ?s)))

(rule (заканчивается-на-внук ?list)
      (reverse ?list (внук . ?any)))

(rule ((пра . ?rel) ?old ?young)
      (and (сын-отца ?young-father ?young)
           (?rel ?old ?young-father)))