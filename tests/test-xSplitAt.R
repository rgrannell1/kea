
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSplit')

	forall(
		"splitting an empty collection yields the empty list",
		test_cases$nonnegative_with_collection_zero,
		xSplit(num, coll) %equals% list()
	)

	forall(
		"splitting with 0 yields an empty list and the list",
		test_cases$collection,
		xSplit(0, coll) %equals% list(list(), as.list(coll)),
		given =
			length(coll) > 0
	)

	forall(
		"splitting with a large number yields the list and an empty list",
		test_cases$collection,
		xSplit(length(coll) + 1, coll) %equals% list(as.list(coll), list()),
		given =
			length(coll) > 0
	)

message('arrow $ xSplit')

	forall(
		"collection $ xSplit",
		test_cases$collection,
		x_(coll)$xSplit(0)$x_() %equals% list(list(), as.list(coll)),
		given =
			length(coll) > 0
	)

message('arrow $ x_Split')

	forall(
		"collection $ x_Split",
		test_cases$collection,
		x_(coll)$x_Split(0) %equals% list(list(), as.list(coll)),
		given =
			length(coll) > 0
	)
