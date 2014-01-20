
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xNotNull')

	forall(
		"not false of empty is logical(0)",
		test_cases$collection_zero,
		is.logical(xNotNull(coll)) && length(xNotNull(coll)) == 0
	)