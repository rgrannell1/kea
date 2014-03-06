
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xWhere")

	forall(
		"the empty collection always yields integer(0)",
		test_cases$collection_zero,
		xWhere(coll) %equals% integer(0)
	)

	forall(
		"true yields 1",
		test_cases$collection_zero,
		xWhere(True) %equals% 1L
	)

	forall(
		"false yields int(0)",
		test_cases$collection_zero,
		xWhere(False) %equals% integer(0)
	)

	forall(
		"na yields int(0)",
		test_cases$collection_zero,
		xWhere(Na) %equals% integer(0)
	)
