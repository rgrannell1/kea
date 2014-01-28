
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xNotNull")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotNull(coll),
		given =
			!is.null(coll)
	)
