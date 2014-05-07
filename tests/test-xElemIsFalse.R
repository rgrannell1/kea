
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xElemIsFalse')

	forall(
		"the empty collection is logical 0",
		test_cases$collection_zero,
		xElemIsFalse(list()) %equals% logical(0)
	)
