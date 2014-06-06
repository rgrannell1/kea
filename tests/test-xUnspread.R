
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xUnspread")

	forall(
		"all base functions become unary with xUnspread",
		test_cases$base_function,
			length( formals(xUnspread(fn) )) == 1
	)

	forall(
		"xUnspread of plus works properly",
		test_cases$sum_over_integers,
		all(xUnspread(fn)( list(coll, coll) ) == coll + coll)
	)
