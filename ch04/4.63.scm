(rule (внук ?f ?g)
      (and (сын ?f ?s)
           (сын ?s ?g)))

(rule (сын-отца ?f ?s)
      (and (жена ?f ?w)
           (сын ?w ?s)))