
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


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
