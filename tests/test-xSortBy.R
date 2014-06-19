
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xSortBy")

	forall(
		"sorting an empty collection is list()",
		test_cases$collection_zero,
		xSortBy(xI, coll) %is% list()
	)

	forall(
		"sorting an empty collection is list()",
		test_cases$collection_zero,
		xSortBy(xI, coll) %is% list()
	)

	forall(
		"sorting an length one is as list(coll)",
		test_cases$num_positive_integer,
		xSortBy(xI, num) %is% list(num)
	)

	forall(
		"sorting a list of integers is same as sort",
		test_cases$positive_integers,
		xSortBy(xI, coll) %is% as.list(sort(coll))
	)

	forall(
		"sorting an length one is as list(coll)",
		test_cases$num_positive_integer,
		xSortBy(xI, rep(num, num)) %is% as.list(rep(num, num))
	)
