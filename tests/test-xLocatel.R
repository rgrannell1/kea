
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xLocatel")

	forall(
		"the empty collection yields integer(0)",
		test_cases$logical_functions_with_collection_zero,
		xLocatel(fn, coll) %equals% integer(0)
	)

	forall(
		"a false function yields integer(0)",
		test_cases$falsity_with_coll,
		xLocatel(fn, coll) %equals% integer(0)
	)

	forall(
		"a true function yields 1",
		test_cases$truth_with_coll,
		xLocatel(fn, coll) == 1,
		given =
			length(coll) > 0
	)

message("arrow $ xLocatel")

	forall(
		"coll $ xLocatel",
		test_cases$truth_with_coll,
		x_(coll)$xLocatel(fn)$x() == 1,
		given =
			length(coll) > 0
	)

	forall(
		"fn $ xLocatel",
		test_cases$truth_with_coll,
		x_(fn)$xLocatel(coll)$x() == 1,
		given =
			length(coll) > 0
	)
