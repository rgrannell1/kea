
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xDuplicatesOf')

	forall(
		"duplicate of empty coll is empty list",
		test_cases$collection_zero,
		xDuplicatesOf(coll) %equals% list()
	)

	forall(
		"union and duplicate contain all elements ",
		test_cases$integers,
		length(coll) == length(unique(coll)) + length(xDuplicatesOf(coll))
	)

