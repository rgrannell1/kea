
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xUnion")

	forall(
		"the union of an empty set is an empty set",
		list(),
		xUnion(list()) %equals% list()
	)

	forall(
		"the union of empty sets is the empty set",
		test_cases$collection_zero,
		xUnion(list(coll, coll)) %equals% list()
	)

message("arrow $ xUnion")
