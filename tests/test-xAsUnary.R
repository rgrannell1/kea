
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xAsUnary")

	forall(
		"all base functions become unary with xAsUnary",
		test_cases$base_function,
			length( formals(xAsUnary(fn) )) == 1
	)

	forall(
		"xAsUnary of plus works properly",
		test_cases$sum_over_integers,
		all(xAsUnary(fn)( list(coll, coll) ) == coll + coll)
	)
