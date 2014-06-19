
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xDrop')

	forall(
		"the empty collection always yields the empty list.",
		test_cases$nonnegative_with_collection_zero,
		xDrop(num, coll) %is% list()
	)

	forall(
		"dropping yields the correct collection.",
		test_cases$positive_with_collection,
		{
			ind <- min(length(coll), num)
			xDrop(num, coll) %is% as.list(tail(coll, -ind))
		},
		given =
			length(coll) > 0
	)
