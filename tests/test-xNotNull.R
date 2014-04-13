
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xNotNull")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotNull(coll),
		given =
			!is.null(coll)
	)

	forall(
		"not null of null is true",
		list(),
		!xNotNull(Null)
	)
