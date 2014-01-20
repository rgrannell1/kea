
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xNotNan')

	forall(
		"not false of empty is logical(0)",
		test_cases$collection_zero,
		is.logical(xNotNan(coll)) && length(xNotNan(coll)) == 0
	)