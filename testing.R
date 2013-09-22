
data <- 
	x_(1:10)$
	xSetProd(1:10)$
	xMap(
	    xAsUnary(
			(a : b) := {
				a^{a + 1} + b^{b + 1}
			}
		)
	)$
	xSelect(x := {
	    x %% 12 == 0
	})$
	xReduce("+")$
	xUnfoldl(
		x := { c(x, x +1) }
	)$
	x()
