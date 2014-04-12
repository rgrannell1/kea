
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xUnzipKeys")

	forall(
		"xUnzipKeys of the empty collection is list()",
		test_cases$collection_zero,
		xUnzipKeys(coll) %equals% list()
	)
