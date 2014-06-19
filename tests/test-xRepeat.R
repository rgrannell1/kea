
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xRepeat")

	forall(
		"repeating the empty list yields the empty list.",
		test_cases$positive_with_collection_zero,
		xRepeat(num, coll) %is% list()
	)

	forall(
		"repeating a collection is done by end-to-end concatenation.",
		test_cases$positive_with_collection,
		xRepeat(num, coll) %is% as.list(rep(coll, num))
	)
