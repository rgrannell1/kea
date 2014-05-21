
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xIsEmpty')

	forall(
		"is empty of empty is True",
		test_cases$collection_zero,
		xIsEmpty(coll)
	)

	forall(
		"is empty of empty is True",
		test_cases$collection,
		!xIsEmpty(coll),
		given =
			length(coll) > 0
	)
