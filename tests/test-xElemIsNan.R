
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xElemIsNan')

	forall(
		"a vector of nan's is nan",
		test_cases$nans,
		all(xElemIsNan(coll))
	)

