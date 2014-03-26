
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xSplitWith")

	forall(
		"splitting with truth is as list",
		test_cases$collection_zero,
		xSplitWith(Truth, list()) %equals% list()
	)

	forall(
		"splitting with truth is as list",
		test_cases$collection,
		xSplitWith(Truth, coll) %equals% lapply(coll, list),
		given =
			length(coll) > 0
	)

	forall(
		"splitting with truth is list(coll)",
		test_cases$collection,
		xSplitWith(Falsity, coll) %equals% list(list(coll)),
		given =
			length(coll) > 0
	)
