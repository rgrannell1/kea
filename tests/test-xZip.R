
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xZip")

	forall(
		"xZip of the empty collection is list()",
		test_cases$collection_zero,
		xZip(coll) %equals% list()
	)
