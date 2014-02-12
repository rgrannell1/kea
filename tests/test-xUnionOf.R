
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xUnionOf")

	forall(
		"the union of an empty set is an empty set",
		list(),
		xUnionOf(list()) %equals% list()
	)
