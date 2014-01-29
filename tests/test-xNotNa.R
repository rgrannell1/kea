
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xNotNa")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotNa(coll)
	)
