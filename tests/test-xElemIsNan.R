
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xElemIsNan')

	forall(
		"the empty collection is logical 0",
		test_cases$collection_zero,
		xElemIsNan(list()) %equals% logical(0)
	)

