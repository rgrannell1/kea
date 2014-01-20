
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xNotFalse')

	forall(
		"not false of empty is logical(0)",
		test_cases$collection_zero,
		is.logical(xNotFalse(coll)) && length(xNotFalse(coll)) == 0
	)
