
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xReject")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xReject(fn, coll) %equals% list()
	)

	forall(
		"a truth function is list unit for collection.",
		test_cases$truth_with_coll,
		expect =
			xReject(fn, coll) %equals% list(),
		given =
			length(coll) > 0
	)

	forall(
		"a falsity function is list identity for collection.",
		test_cases$falsity_with_coll,
		xReject(fn, coll) %equals% as.list(coll)
	)

	forall(
		"a na function is list identity for collection.",
		test_cases$moot_with_coll,
		xReject(fn, coll) %equals% as.list(coll)
	)

	forall(
		"selecting the odd-numbers works as expected, and ordering is preserved.",
		test_cases$mod2_over_ints,
		xReject(fn, coll) %equals% as.list(coll[coll %% 2 == 1])
	)

message("arrow $ xReject")

	forall(
		"collection.xReject selects odd-numbers.",
		test_cases$mod2_over_ints,
		x_(coll)$xReject(fn)$x() %equals%
			as.list(coll[coll %% 2 == 1])
	)

	forall(
		"function.xReject selects odd-numbers.",
		test_cases$mod2_over_ints,
		x_(fn)$xReject(coll)$x() %equals%
			as.list(coll[coll %% 2 == 1])
	)

