
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xProdSetOf")

	forall(
		"setprod of any empty collection is empty list",
		test_cases$two_collections,
		xProdSetOf(list(coll1, coll2)) %is% list(),
		given =
			prod(length(coll1), length(coll2)) == 0
	)

	forall(
		"the length is the product of the set length",
		test_cases$two_collections,
		length( xProdSetOf(list(coll1, coll2)) ) ==
		prod(length(coll1), length(coll2)),
		given =
			prod(length(coll1), length(coll2)) > 0
	)
