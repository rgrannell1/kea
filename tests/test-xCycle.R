
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xCycle')

	forall(
		"cycling the empty collection is the empty collection",
		test_cases$integer_with_collection_zero,
		xCycle(num, coll) %equals% list()
	)

	forall(
		"cycling with zero yields the correct collection.",
		test_cases$collection,
		xCycle(0, list(coll)) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
 	)
