
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

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

	forall(
		"xList can support multiple bindings",
		test_cases$positive_integers,
		xList[ x + y, x <- coll, y <- coll ] %equals%
			as.list(apply(expand.grid(coll, coll), 1, sum))
	)
