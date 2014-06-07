
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xUnzipKeys")

	forall(
		"xUnzipKeys of the empty collection is list()",
		test_cases$collection_zero,
		xUnzipKeys(coll) %equals% list()
	)
