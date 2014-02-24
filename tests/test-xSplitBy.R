
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xSplitBy")

	forall(
		"splitting with truth is as list",
		test_cases$collection_zero,
		xSplitBy(Truth, list()) %equals% list()
	)

	forall(
		"splitting with truth is as list",
		test_cases$collection,
		xSplitBy(Truth, coll) %equals% lapply(coll, list),
		given =
			length(coll) > 0
	)

	forall(
		"splitting with truth is list(coll)",
		test_cases$collection,
		xSplitBy(Falsity, coll) %equals% list(list(coll)),
		given =
			length(coll) > 0
	)
