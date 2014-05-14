
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xElemNotNull')

	forall(
		"the empty collection is logical 0",
		test_cases$collection_zero,
		xElemNotNull(list()) %equals% logical(0)
	)

	forall(
		"elemnotnull of null is false",
		list(),
		xElemNotNull(list(NULL)) == False
	)
	