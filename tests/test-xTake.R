
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xTake')

	forall(
		"taking from the empty collection is the empty collection",
		test_cases$nonnegative_with_collection_zero,
		x_(coll)$xTake(num)$x_() %equals% list()
	)

	forall(
		"taking yields the correct collection.",
		test_cases$positive_with_collection,
		{
			ind <- min(length(coll), num)
			xTake(num, coll) %equals% as.list(head(coll, ind))
		},
		given =
			length(coll) > 0
	)

