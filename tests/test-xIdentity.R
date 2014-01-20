
message("xIdentity")

	forall(
		"identity returns the collection.",
		test_cases$collection,
		xIdentity(coll) %equals% coll
	)
