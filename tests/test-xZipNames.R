
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xZipNames")

	forall(
		"xZipNames of the empty collection is list()",
		test_cases$collection_zero,
		xZipNames(coll) %equals% list()
	)
