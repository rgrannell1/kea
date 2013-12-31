
forall <- arrow:::forall
test_cases <- arrow:::test_cases

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

message("arrow $ xMap")

	forall(
		"collection $ xMap selects even-numbers.",
		test_cases$succ_over_integers,
		all( unlist(x_(coll)$xMap(fn)$x_()) == unlist(coll) + 1 )
	)

	forall(
		"function $ xMap selects even-numbers.",
		test_cases$succ_over_integers,
		all( unlist(x_(fn)$xMap(coll)$x_()) == unlist(coll) + 1 )
	)

message("arrow $ xMap...")

message("arrow $ x_Map")

	forall(
		"collection $ x_Map selects even-numbers.",
		test_cases$succ_over_integers,
		all( unlist(x_(coll)$x_Map(fn)) == unlist(coll) + 1 )
	)

	forall(
		"function $ x_Map selects even-numbers.",
		test_cases$succ_over_integers,
		all( unlist(x_(fn)$x_Map(coll)) == unlist(coll) + 1 )
	)