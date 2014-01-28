
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xElemNotFalse')

	forall(
		"not false of empty is logical(0)",
		test_cases$collection_zero,
		is.logical(xElemNotFalse(coll)) && length(xElemNotFalse(coll)) == 0
	)
