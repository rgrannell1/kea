
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xElemNotFalse')

	forall(
		"the empty collection is logical 0",
		test_cases$collection_zero,
		xElemNotFalse(list()) %equals% logical(0)
	)
