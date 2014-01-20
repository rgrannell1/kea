
message("xRemoveNa")

	forall(
		"removena of the empty collection is list()",
		test_cases$collection_zero,
		xRemoveNa(coll) %equals% list()
	)

