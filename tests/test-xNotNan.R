
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xNotNan")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotNan(coll)
	)
