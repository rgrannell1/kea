
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xOneOf")

	forall(
		"one of the empty collection returns the empty list",
		test_cases$collection_zero,
		xOneOf(coll) %equals% list()
	)

	forall(
		"one of the empty collection is in the collection",
		test_cases$collection,
		xOneOf(coll) %in% coll,
		given =
			length(coll) > 0
	)
