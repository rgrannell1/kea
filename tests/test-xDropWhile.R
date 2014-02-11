
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xDropWhile")

	forall(
		"dropwhile with truth returns the collection.",
		test_cases$truth_with_coll,
		xDropWhile(fn, coll) %equals% list()
	)

	forall(
		"dropwhile with falsity returns the collection.",
		test_cases$falsity_with_coll,
		xDropWhile(fn, coll) %equals% as.list(coll)
	)

	forall(
		"dropwhile with moot returns the collection.",
		test_cases$moot_with_coll,
		xDropWhile(fn, coll) %equals% as.list(coll)
	)
