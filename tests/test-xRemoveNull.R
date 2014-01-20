
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xRemoveNull")

	forall(
		"removenull of the empty collection is list()",
		test_cases$collection_zero,
		xRemoveNull(coll) %equals% list()
	)
