;;I think it is impossible. We can rearrange definitions and try toWe can 
;;eval those of them who depend on other later (and it will work for
;;ex in 4.19), but definitions can depend on each other (as in
;;ex. 4.18), so rearrangment will not help. Delaying evaluation will
;;not help ether. Another way - is to assign to every variable default
;;value, like nil, but we will not get 20 in this case.