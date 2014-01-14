
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xIsNan')

	forall(
		"a vector of nan's is nan",
		test_cases$nans,
		all(xIsNan(coll))
	)

