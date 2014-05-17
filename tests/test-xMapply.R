
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xMapply")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xMapply(fn, coll) %equals% list()
	)

