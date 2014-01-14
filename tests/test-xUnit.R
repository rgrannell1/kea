
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xUnit')

	forall(
		"unit of pairlist is Null",
		list(),
		xUnit(pairlist()) %equals% Null
	)

	forall(
		"the unit of a collection is the same type as the collection",
		test_cases$collection,
		typeof( xUnit(coll) ) == typeof(coll)
	)

	forall(
		"the unit of a collection is length zero",
		test_cases$collection,
		length( xUnit(coll) ) == 0
	)

