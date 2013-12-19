
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xReverse')

	forall(
		"reversing the empty list is the empty collection",
		test_cases$collection_zero,
		xReverse(coll) %equals% list()
	)

	forall(
		"reversing a collection is the reversed collection",
		test_cases$collection,
		xReverse(coll) %equals% as.list(rev(coll))
	)

message('arrow $ xReverse')

	forall(
		"collection $ xReverse",
		test_cases$collection,
		x_(coll)$xReverse()$x() %equals% as.list(rev(coll))
	)

message('arrow $ x_Reverse')

	forall(
		"collection $ x_Reverse",
		test_cases$collection,
		x_(coll)$x_Reverse() %equals% as.list(rev(coll))
	)
