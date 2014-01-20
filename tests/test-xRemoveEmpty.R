
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xRemoveEmpty")

	forall(
		"removeempty of the empty collection is list()",
		test_cases$collection_zero,
		xRemoveEmpty(coll) %equals% list()
	)
