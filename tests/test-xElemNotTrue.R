
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xElemNotTrue')

	forall(
		"the empty collection is logical 0",
		test_cases$collection_zero,
		xElemNotTrue(list()) %equals% logical(0)
	)
