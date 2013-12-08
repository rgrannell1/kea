
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xLocater")

	forall(
		"the empty collection yields integer(0)",
		test_cases$logical_functions_with_collection_zero,
		xLocater(fn, coll) %equals% integer(0)
	)

	forall(
		"a false function yields integer(0)",
		test_cases$falsity_with_coll,
		xLocater(fn, coll) %equals% integer(0)
	)

	forall(
		"a true function yields length(coll)",
		test_cases$truth_with_coll,
		xLocater(fn, coll)== length(coll),
		given =
			length(coll) > 0
	)

message("arrow $ xLocater")

	forall(
		"coll $ xLocater",
		test_cases$truth_with_coll,
		x_(coll)$xLocater(fn)$x() == length(coll),
		given =
			length(coll) > 0
	)

	forall(
		"fn $ xLocater",
		test_cases$truth_with_coll,
		x_(fn)$xLocater(coll)$x() == length(coll),
		given =
			length(coll) > 0
	)
