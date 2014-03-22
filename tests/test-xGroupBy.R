
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xGroupBy")

	forall(
		"grouping an empty collection is list()",
		test_cases$collection_zero,
		xGroupBy(xI, coll) %equals% list()
	)

	forall(
		"grouping an length one is as list(coll)",
		test_cases$num_positive_integer,
		xGroupBy(xI, num) %equals% list(list(num, list(num)))
	)
