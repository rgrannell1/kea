
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xReverse')

	forall(
		"reversing the empty list is the empty collection",
		test_cases$collection_zero,
		xReverse(coll) %is% list()
	)

	forall(
		"reversing a collection is the reversed collection",
		test_cases$collection,
		xReverse(coll) %is% as.list(rev(coll))
	)
