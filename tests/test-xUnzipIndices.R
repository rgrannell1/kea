
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xUnzipIndices")

	forall(
		"xUnzipIndices of the empty collection is list()",
		test_cases$collection_zero,
		xUnzipIndices(coll) %equals% list()
	)
