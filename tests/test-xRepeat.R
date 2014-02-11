
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xRepeat")

	forall(
		"repeating the empty list yields the empty list.",
		test_cases$positive_with_collection_zero,
		xRepeat(num, coll) %equals% list()
	)

	forall(
		"repeating a collection is done by end-to-end concatenation.",
		test_cases$positive_with_collection,
		xRepeat(num, coll) %equals% as.list(rep(coll, num))
	)
