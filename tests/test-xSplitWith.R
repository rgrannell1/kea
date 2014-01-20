
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xSplitWith")

	forall(
		"splitting with truth is empty and coll",
		test_cases$truth_with_coll,
		xSplitWith(fn, coll) %equals% list(list(), as.list(coll)),
		given =
			length(coll) > 0
	)

	forall(
		"splitting with false is coll and empty",
		test_cases$falsity_with_coll,
		xSplitWith(fn, coll) %equals% list(as.list(coll), list()),
		given =
			length(coll) > 0
	)

	forall(
		"splitting with na is coll and empty",
		test_cases$moot_with_coll,
		xSplitWith(fn, coll) %equals% list(as.list(coll), list()),
		given =
			length(coll) > 0
	)
