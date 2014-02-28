
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xOrderOf")

	forall(
		"order of empty collection is integer(0)",
		test_cases$collection_zero,
		xOrderOf(coll) %equals% integer(0)
	)