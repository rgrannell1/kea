
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xMapply")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xMapply(fn, coll) %equals% list()
	)

