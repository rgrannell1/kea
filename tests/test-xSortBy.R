
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xSortBy")

	forall(
		"sorting an empty collection is list()",
		test_cases$collection_zero,
		xSortBy(xI, coll) %equals% list()
	)

	forall(
		"sorting an empty collection is list()",
		test_cases$collection_zero,
		xSortBy(xI, coll) %equals% list()
	)

	forall(
		"sorting an length one is as list(coll)",
		test_cases$num_positive_integer,
		xSortBy(xI, num) %equals% list(num)
	)

	forall(
		"sorting a list of integers is same as sort",
		test_cases$positive_integers,
		xSortBy(xI, coll) %equals% as.list(sort(coll))
	)

	forall(
		"sorting an length one is as list(coll)",
		test_cases$num_positive_integer,
		xSortBy(xI, rep(num, num)) %equals% as.list(rep(num, num))
	)
