
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

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
		xIsNull(Null)
	)
