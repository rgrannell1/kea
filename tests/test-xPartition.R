
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xPartition")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xPartition(fn, coll) %equals% list()
	)

	forall(
		"a truth function is [list collection, list unit].",
		test_cases$truth_with_coll,
		expect =
			xPartition(fn, coll) %equals%
				list( as.list(coll), list() ),
		given =
			length(coll) > 0
	)

	forall(
		"a falsity function is [list unit, list collection].",
		test_cases$falsity_with_coll,
		xPartition(fn, coll) %equals%
			list( list(), as.list(coll) ),
		given =
			length(coll) > 0
	)

	forall(
		"a na function is [list unit, list collection].",
		test_cases$moot_with_coll,
		xPartition(fn, coll) %equals%
			list( list(), as.list(coll) ),
		given =
			length(coll) > 0
	)

	forall(
		"partitioning the integers by evenness works as expected, and ordering is preserved.",
		test_cases$mod2_over_ints,
		xPartition(fn, coll) %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)

message("arrow $ xPartition")

	forall(
		"collection $ xPartition partitions into even and odd-numbers.",
		test_cases$mod2_over_ints,
		x_(coll)$xPartition(fn)$x() %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)

	forall(
		"function $ xPartition partitions into even and odd-numbers.",
		test_cases$mod2_over_ints,
		x_(fn)$xPartition(coll)$x() %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)

message("arrow $ x_PartitionWith")

	forall(
		"collection $ x_PartitionWith",
		test_cases$mod2_over_ints,
		x_(coll)$x_PartitionWith(fn) %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)

	forall(
		"function $ x_PartitionWith",
		test_cases$mod2_over_ints,
		x_(fn)$x_PartitionWith(coll) %equals%
			list(
				as.list(coll[coll %% 2 == 0]),
				as.list(coll[coll %% 2 == 1]) ),
		given =
			length(coll) > 0
	)
