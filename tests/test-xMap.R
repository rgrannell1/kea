
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xMap")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xMap(fn, coll) %equals% list()
	)

	forall(
		"mapping identity over the list preserves its contents.",
		test_cases$collection,
		xMap(identity, coll) %equals% as.list(coll)
	)

	forall(
		"mapping identity over the list preserves its length.",
		test_cases$collection,
		length(xMap(identity, coll)) == length(coll)
	)

	forall(
		"mapping increment increments the list",
		test_cases$succ_over_integers,
		all( unlist(xMap(fn, coll)) == unlist(coll) + 1 )
	)

