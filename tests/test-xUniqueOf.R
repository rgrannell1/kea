
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xUniqueOf')

	forall(
		"unique of empty coll is empty list",
		test_cases$collection_zero,
		xUniqueOf(coll) %equals% list()
	)

	forall(
		"union and duplicate contain all elements ",
		test_cases$integers,
		length(coll) == length(xUniqueOf(coll)) + length( which(duplicated(coll)) )
	)

