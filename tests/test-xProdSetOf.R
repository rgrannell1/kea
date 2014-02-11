
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xProdSetOf")

	forall(
		"setprod of any empty collection is empty list",
		test_cases$two_collections,
		xProdSetOf(list(coll1, coll2)) %equals% list(),
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
