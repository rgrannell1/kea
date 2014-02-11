
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xRemoveNa")

	forall(
		"removena of the empty collection is list()",
		test_cases$collection_zero,
		xRemoveNa(coll) %equals% list()
	)

