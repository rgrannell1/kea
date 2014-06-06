
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)


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
