
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xUnionOf")

	forall(
		"the union of an empty set is an empty set",
		list(),
		xUnionOf(list()) %equals% list()
	)

	forall(
		"the union of two equals sets is the first set",
		test_cases$collection,
		xUnionOf_(unique(coll), unique(coll)) %equals% unique(coll)
	)