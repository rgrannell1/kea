
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xUnion")

	forall(
		"the union of an empty set is an empty set",
		list(),
		xUnion(list()) %equals% list()
	)
