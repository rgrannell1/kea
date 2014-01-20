
message("xRemoveEmpty")

	forall(
		"removeempty of the empty collection is list()",
		test_cases$collection_zero,
		xRemoveEmpty(coll) %equals% list()
	)
