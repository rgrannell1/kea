
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)


message("xNotFalse")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotFalse(coll)
	)

	forall(
		"not Null of Null is false",
		list(),
		!xNotNull(Null)
	)
