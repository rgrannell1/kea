
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xTakeWhile")

	forall(
		"takewhile with truth returns the collection.",
		test_cases$truth_with_coll,
		xTakeWhile(fn, coll) %equals% as.list(coll)
	)

	forall(
		"takewhile with falsity returns the collection.",
		test_cases$falsity_with_coll,
		xTakeWhile(fn, coll) %equals% list()
	)

	forall(
		"takewhile with moot returns the collection.",
		test_cases$moot_with_coll,
		xTakeWhile(fn, coll) %equals% list()
	)

