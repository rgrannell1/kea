
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xRejectEmpty")

	forall(
		"removeempty of the empty collection is list()",
		test_cases$collection_zero,
		xRejectEmpty(coll) %equals% list()
	)
