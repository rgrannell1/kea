
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xNotNan')

	forall(
		"a vector of nan's is nan",
		test_cases$nans,
		!any(xNotNan(coll))
	)

