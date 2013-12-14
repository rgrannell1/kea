
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xInter")

	forall(
		"the intersection with an empty collection is an empty list",
		test_cases$two_collection_zeros,
		xInter(list(coll1, coll2)) %equals% list(),
		given =
			length(coll1) == 0 || length(coll2) == 0
	)

message("arrow $ xInter")

	forall(
		"collection $ xInter",
		test_cases$two_collection_zeros,
		x_(coll1)$xInter()$x() %equals% list(),
		given =
			length(coll1) == 0 || length(coll2) == 0
	)

message("arrow $ xInter...")

	forall(
		"collection $ xInter",
		test_cases$two_collection_zeros,
		x_(coll1)$xInter...(coll2)$x() %equals% list(),
		given =
			length(coll1) == 0 || length(coll2) == 0
	)