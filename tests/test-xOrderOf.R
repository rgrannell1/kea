
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)


message("xOrderOf")

	forall(
		"order of empty collection is integer(0)",
		test_cases$collection_zero,
		xOrderOf(coll) %is% integer(0)
	)
