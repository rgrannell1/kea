
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xNotEmpty')

	forall(
		"not empty of empty is True",
		test_cases$collection_zero,
		!xNotEmpty(coll)
	)

	forall(
		"not empty of empty is True",
		test_cases$collection,
		xNotEmpty(coll),
		given =
			length(coll) > 0
	)
