
forall <- arrow:::forall
test_cases <- arrow:::test_cases

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

message('arrow $ xDrop')

	forall(
		"dropping yields the correct collection.",
		test_cases$positive_with_collection,
		{
			ind <- min(length(coll), num)
			x_(coll)$xDrop(num)$x() %equals% as.list(tail(coll, -ind))
		},
		given =
			length(coll) > 0
	)
message('arrow $ x_Drop')

	forall(
		"dropping yields the correct collection.",
		test_cases$positive_with_collection,
		{
			ind <- min(length(coll), num)
			x_(coll)$x_Drop(num) %equals% as.list(tail(coll, -ind))
		},
		given =
			length(coll) > 0
	)
