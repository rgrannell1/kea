
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xPartition")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xPartition(fn, coll) %is% list()
	)

	forall(
		"a truth function is [list collection, list unit].",
		test_cases$truth_with_coll,
		expect =
			xPartition(fn, coll) %is%
				list( as.list(coll), list() ),
		given =
			length(coll) > 0
	)

	forall(
		"a falsity function is [list unit, list collection].",
		test_cases$falsity_with_coll,
		xPartition(fn, coll) %is%
			list( list(), as.list(coll) ),
		given =
			length(coll) > 0
	)

	forall(
		"a na function is [list unit, list collection].",
		test_cases$moot_with_coll,
		xPartition(fn, coll) %is%
			list( list(), as.list(coll) ),
		given =
			length(coll) > 0
	)

	forall(
		"partitioning the integers by evenness works as expected, and ordering is preserved.",
		test_cases$mod2_over_ints,
		xPartition(fn, coll) %is%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)
