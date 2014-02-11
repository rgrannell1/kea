
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xFlatMap")

	forall(
		"the empty collection always yields the empty list.",
		test_cases$logical_functions_with_collection_zero,
		xFlatMap(fn, coll) %equals% list()
	)

	forall(
		"flatmapping identity over the list preserves its contents.",
		test_cases$collection,
		xFlatMap(identity, coll) %equals% as.list(coll)
	)

	forall(
		"flatmapping increment increments the list",
		test_cases$succ_over_integers,
		all( unlist(xFlatMap(fn, coll)) == unlist(coll) + 1 )
	)

	forall(
		"flatmapping can extend a collection length.",
		test_cases$integers,
		length(xFlatMap(function (x) c(x, x), coll)) == 2 * length(coll)
	)
