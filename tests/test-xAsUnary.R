
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xAsUnary")

	forall(
		"all functions become unary",
		test_cases$base_function,
		{
			g <- xAsUnary(fn)
			length(formals(g)) == 1
		}
	)

	forall(
		"addition is defined",
		test_cases$sum_over_integers,
		all(xAsUnary(fn)( list(coll, coll) ) == coll + coll)
	)

message("arrow $ xAsUnary")

	forall(
		"function $ xAsUnary",
		test_cases$sum_over_integers,
		all(x_(fn)$xAsUnary()$x()( list(coll, coll) ) == coll + coll)
	)

message("arrow $ x_AsUnary")

	forall(
		"function $ x_AsUnary",
		test_cases$sum_over_integers,
		all(x_(fn)$x_AsUnary()( list(coll, coll) ) == coll + coll)
	)
