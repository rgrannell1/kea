
message("xRemoveNull")

	forall(
		"removenull of the empty collection is list()",
		test_cases$collection_zero,
		xRemoveNull(coll) %equals% list()
	)
