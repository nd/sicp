env for operator and operands
argl for each operand
proc for operand sequence

(f 'x 'y)
;;we don't have to save env for operator, because f is a variable and variable lookup in env doesn't change env
;;we don't have to save env for operands, because they don't change it;
;;we don't have to save proc before evaluating operand sequence, becuase evaluating of arg don't change proc
;;we don't have to save argl for each operand, we can assing operands to argl, because they are salf-evaluating

((f) 'x 'y)
;;we don't have to save env for operands, because they don't change it;
;;we don't have to save proc before evaluating operand sequence, becuase evaluating of arg don't change proc
;;we don't have to save argl for each operand, we can assing operands to argl, because they are salf-evaluating

(f (g 'x) y)
;;we don't have to save env for operator, because f is a variable and variable lookup in env doesn't change env

(f (g 'x) 'y)
;;we don't have to save env for operator, because f is a variable and variable lookup in env doesn't change env
