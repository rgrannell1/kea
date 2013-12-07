
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xPartitionWith")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xPartitionWith(fn, coll) %equals% list()
	)

	forall(
		"a truth function is [list collection, list unit].",
		test_cases$truth_with_coll,
		expect =
			xPartitionWith(fn, coll) %equals%
				list( as.list(coll), list() ),
		given =
			length(coll) > 0
	)

	forall(
		"a falsity function is [list unit, list collection].",
		test_cases$falsity_with_coll,
		xPartitionWith(fn, coll) %equals%
			list( list(), as.list(coll) ),
		given =
			length(coll) > 0
	)

	forall(
		"a na function is [list unit, list collection].",
		test_cases$moot_with_coll,
		xPartitionWith(fn, coll) %equals%
			list( list(), as.list(coll) ),
		given =
			length(coll) > 0
	)

	forall(
		"partitioning the integers by evenness works as expected, and ordering is preserved.",
		test_cases$mod2_over_ints,
		xPartitionWith(fn, coll) %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)

message("arrow $ xPartitionWith")

	forall(
		"collection $ xPartitionWith partitions into even and odd-numbers.",
		test_cases$mod2_over_ints,
		x_(coll)$xPartitionWith(fn)$x() %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)

	forall(
		"function $ xPartitionWith partitions into even and odd-numbers.",
		test_cases$mod2_over_ints,
		x_(fn)$xPartitionWith(coll)$x() %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)
