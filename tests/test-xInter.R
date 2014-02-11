
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xInter")

	forall(
		"the intersection with an empty collection is an empty list",
		test_cases$two_collection_zeros,
		xInter(list(coll1, coll2)) %equals% list(),
		given =
			length(coll1) == 0 || length(coll2) == 0
	)

