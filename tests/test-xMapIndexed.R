
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xMapIndexed")

	forall(
		"mapindexed the empty collection always yields the empty list.",
		test_cases$id_over_collection_zero,
		xMapIndexed(fn, coll) %equals% list()
	)

	forall(
		"mapindexed's values are correct",
		test_cases$left_over_collection,
		xMapIndexed(fn, coll) %equals% as.list(coll)
	)

	forall(
		"mapindexed's indices are correct",
		test_cases$right_over_collection,
		xMapIndexed(fn, coll) %equals% as.list(seq_along(coll))
	)

	forall(
		"mapindexed's can increment correctly.",
		test_cases$sum_over_integers,
		all( unlist(xMapIndexed(fn, coll)) ==
			coll + seq_along(coll) )
	)

message("arrow $ xMapIndexed")

	forall(
		"collection $ xMapIndexed increments correctly.",
		test_cases$sum_over_integers,
		all( unlist(x_(coll)$xMapIndexed(fn)$x_()) ==
			unlist(coll) + seq_along(coll) )
	)

	forall(
		"function $ xMapIndexed increments correctly.",
		test_cases$sum_over_integers,
		all( unlist(x_(fn)$xMapIndexed(coll)$x_()) ==
			unlist(coll) + seq_along(coll) )
	)

message("arrow $ x_MapIndexed")

	forall(
		"collection $ x_MapIndexed",
		test_cases$sum_over_integers,
		all( unlist(x_(coll)$x_MapIndexed(fn)) ==
			unlist(coll) + seq_along(coll) )
	)

	forall(
		"function $ x_MapIndexed",
		test_cases$sum_over_integers,
		all( unlist(x_(fn)$x_MapIndexed(coll)) ==
			unlist(coll) + seq_along(coll) )
	)
