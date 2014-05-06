
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xElemNotNan')

	forall(
		"not false of empty is logical(0)",
		test_cases$collection_zero,
		is.logical(xElemNotNan(coll)) && length(xElemNotNan(coll)) == 0
	)
