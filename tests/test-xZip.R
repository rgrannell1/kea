
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xZip")

	forall(
		"xZip of the empty collection is list()",
		test_cases$collection_zero,
		xZip(coll) %equals% list()
	)

	forall(
		"xZip a collection is list(collection)",
		test_cases$collection,
		xZip(coll)[[1]] %equals% as.list(coll),
		given =
			length(coll) > 0
	)
