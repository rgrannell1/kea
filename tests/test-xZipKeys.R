
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xZipKeys")

	forall(
		"xZipKeys of the empty collection is list()",
		test_cases$collection_zero,
		xZipKeys(coll) %equals% list()
	)
