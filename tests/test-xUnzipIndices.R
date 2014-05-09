
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xUnzipIndices")

	forall(
		"xUnzipIndices of the empty collection is list()",
		test_cases$collection_zero,
		xUnzipIndices(coll) %equals% list()
	)

	forall(
		"unzipping indiced is length two",
		test_cases$collection,
		all(sapply( xUnzipIndices(coll), length ) == 2)
	)

	forall(
		"the first column is the indices",
		test_cases$collection,
		all(sapply( xUnzipIndices(coll), function (x) x[[1]] ) == seq_along(coll))
	)

	forall(
		"the second column is the values",
		test_cases$collection,
		all( sapply(seq_along(coll), function (ith) {
			identical(coll[[ith]], xUnzipIndices(coll)[[ith]][[2]])
		}) )
	)
