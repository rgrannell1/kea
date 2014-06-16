
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)


message("xInter")

	forall(
		"the intersection with an empty collection is an empty list",
		test_cases$two_collection_zeros,
		xInter(list(coll1, coll2)) %is% list(),
		given =
			length(coll1) == 0 || length(coll2) == 0
	)

