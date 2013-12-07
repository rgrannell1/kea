
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xSelect")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xSelect(fn, coll) %equals% list()
	)

	forall(
		"a truth function is list identity for collection.",
		test_cases$truth_with_coll,
		xSelect(fn, coll) %equals% as.list(coll),
		given =
			length(coll) > 0
	)

	forall(
		"a falsity function is list unit for collection.",
		test_cases$falsity_with_coll,
		xSelect(fn, coll) %equals% list()
	)

	forall(
		"a na function is list unit for collection.",
		test_cases$moot_with_coll,
		xSelect(fn, coll) %equals% list()
	)

	forall(
		"selecting the even-numbers works as expected, and ordering is preserved.",
		test_cases$mod2_over_ints,
		xSelect(fn, coll) %equals% as.list(coll[coll %% 2 == 0])
	)

message("arrow $ xSelect")

	forall(
		"collection $ xSelect selects even-numbers.",
		test_cases$mod2_over_ints,
		x_(coll)$xSelect(fn)$x() %equals%
			as.list(coll[coll %% 2 == 0])
	)

	forall(
		"function $ xSelect selects even-numbers.",
		test_cases$mod2_over_ints,
		x_(fn)$xSelect(coll)$x() %equals%
			as.list(coll[coll %% 2 == 0])
	)

message("arrow $ xSelect...")

