
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xElemNotNull')

	forall(
		"not false of empty is logical(0)",
		test_cases$collection_zero,
		is.logical(xElemNotNull(coll)) && length(xElemNotNull(coll)) == 0,
		given =
			!is.null(coll)
	)