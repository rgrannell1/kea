
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xDrop')

	forall(
		"the empty collection always yields the empty list.",
		test_cases$nonnegative_with_collection_zero,
		xDrop(num, coll) %equals% list()
	)

	forall(
		"dropping yields the correct collection.",
		test_cases$positive_with_collection,
		{
			ind <- min(length(coll), num)
			xDrop(num, coll) %equals% as.list(tail(coll, -ind))
		},
		given =
			length(coll) > 0
	)
