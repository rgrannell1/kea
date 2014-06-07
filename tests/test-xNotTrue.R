
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xNotTrue")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotTrue(coll)
	)

	forall(
		"not True of True is false",
		list(),
		!xNotTrue(True)
	)
