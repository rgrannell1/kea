
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xTake')

	forall(
		"taking from the empty collection is the empty collection",
		test_cases$nonnegative_with_collection_zero,
		x_(coll)$xTake(num)$x() %equals% list()
	)







message('arrow $ xTake')

message('collection $ xTake')

	forall(
		"taking from the empty collection is the empty collection.",
		test_cases$nonnegative_with_collection_zero,
		x_(coll)$xTake(num)$x() %equals% list()
	)

message('collection $ xTake...')

	forall(
		"taking from the empty collection is the empty collection.",
		test_cases$nonnegative_with_collection_zero,
		x_(coll)$xTake...(num)$x() %equals% list()
	)

message('collection $ x_Take')

	forall(
		"taking from the empty collection is the empty collection.",
		test_cases$nonnegative_with_collection_zero,
		x_(coll)$x_Take(num) %equals% list()
	)

message('collection $ x_Take...')

	forall(
		"taking from the empty collection is the empty collection.",
		test_cases$nonnegative_with_collection_zero,
		x_(coll)$x_Take...(num) %equals% list()
	)
