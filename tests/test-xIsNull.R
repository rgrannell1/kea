
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xIsNull")

	forall(
		"is null of empty collection is False",
		test_cases$collection_zero,
		!xIsNull(coll),
		given =
			!is.null(coll)
	)

	forall(
		"is null of null is true",
		list(),
		xIsNa(Na)
	)
