
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xList[ ]")

	forall(
		"xList of nothing is the empty list",
		test_cases$collection,
		xList[ ] %equals% list()
	)

	forall(
		"xList of a collection is as.list",
		test_cases$collection,
		xList[ x, x <- coll ] %equals% as.list(coll)
	)

	forall(
		"xList can support predicate with one binding",
		test_cases$collection,
		xList[ x, x <- coll, True ] %equals% as.list(coll)
	)

	forall(
		"xList can support predicate with one binding",
		test_cases$positive_integers,
		xList[ x, x <- coll, x %% 2 == 0 ] %equals% as.list(coll[ coll %% 2 == 0 ]),
		given =
			length(coll) > 0
	)
