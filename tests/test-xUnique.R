
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xUnique')

	forall(
		"unique of empty coll is empty list",
		test_cases$collection_zero,
		xUnique(coll) %equals% list()
	)

	forall(
		"union and duplicate contain all elements ",
		test_cases$integers,
		length(coll) == length(xUnique(coll)) + length( which(duplicated(coll)) )
	)

