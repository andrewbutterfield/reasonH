module NakedExpr where
infix 1 ===; x === y = x == y
nakedExpr = 1+(2+3) === (1+2)+3
