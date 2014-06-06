
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xLocate")

	forall(
		"the empty collection yields integer(0)",
		test_cases$logical_functions_with_collection_zero,
		xLocate(fn, coll) %equals% integer(0)
	)

	forall(
		"a false function yields integer(0)",
		test_cases$falsity_with_coll,
		xLocate(fn, coll) %equals% integer(0)
	)

	forall(
		"a true function yields 1",
		test_cases$truth_with_coll,
		xLocate(fn, coll) %equals% seq_along(coll),
		given =
			length(coll) > 0
	)
