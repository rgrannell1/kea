
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xIsTrue")

	forall(
		"istrue of empty collection is False",
		test_cases$collection_zero,
		!xIsTrue(coll)
	)
