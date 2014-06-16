
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xChoose")

	forall(
		"combos of empty collection is empty list",
		test_cases$nonnegative_with_collection_zero,
		xChoose(num, coll) %is% list()
	)

