
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xZipWith")

	forall(
		"zipwithing the empty collection is the empty list.",
		test_cases$collection_zero,
		xZipWith(identity, coll) %equals% list()
	)

	forall(
		"zipwithing the sum across two lists is the sum.",
		test_cases$sum_over_integers,
		xZipWith(fn, list(coll, coll)) %equals% as.list(2 * unlist(coll))
	)

