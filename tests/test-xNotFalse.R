
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xNotFalse")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotFalse(coll)
	)
