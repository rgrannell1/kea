
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSplitAt')

	forall(
		"splitting an empty collection yields the empty list",
		test_cases$nonnegative_with_collection_zero,
		xSplitAt(num, coll) %equals% list()
	)

	forall(
		"splitting with 0 yields an empty list and the list",
		test_cases$collection,
		xSplitAt(0, coll) %equals% list(list(), as.list(coll)),
		given =
			length(coll) > 0
	)

	forall(
		"splitting with a large number yields the list and an empty list",
		test_cases$collection,
		xSplitAt(length(coll) + 1, coll) %equals% list(as.list(coll), list()),
		given =
			length(coll) > 0
	)

message('arrow $ xSplitAt')

	forall(
		"collection $ xSplitAt",
		test_cases$collection,
		x_(coll)$xSplitAt(0)$x_() %equals% list(list(), as.list(coll)),
		given =
			length(coll) > 0
	)

message('arrow $ x_SplitAt')

	forall(
		"collection $ x_SplitAt",
		test_cases$collection,
		x_(coll)$x_SplitAt(0) %equals% list(list(), as.list(coll)),
		given =
			length(coll) > 0
	)
