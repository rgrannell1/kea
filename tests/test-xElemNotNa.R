
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xElemNotNa')

	forall(
		"not false of empty is logical(0)",
		test_cases$collection_zero,
		is.logical(xElemNotNa(coll)) && length(xElemNotNa(coll)) == 0
	)