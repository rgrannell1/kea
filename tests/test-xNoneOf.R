
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xNoneOf")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xNoneOf(fn, coll) %equals% logical(0)
	)

	forall(
		"a truth function is false for collection.",
		test_cases$truth_with_coll,
		xNoneOf(fn, coll) == False,
		given =
			length(coll) > 0
	)

	forall(
		"a falsity function is list unit for collection.",
		test_cases$falsity_with_coll,
		xNoneOf(fn, coll) == True,
		given =
			length(coll) > 0
	)

	forall(
		"a na function is list unit for collection.",
		test_cases$moot_with_coll,
		xNoneOf(fn, coll) == True,
		given =
			length(coll) > 0
	)

