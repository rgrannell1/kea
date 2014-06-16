
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xDuplicatesOf')

	forall(
		"duplicate of empty coll is empty list",
		test_cases$collection_zero,
		xDuplicatesOf(coll) %is% list()
	)

	forall(
		"union and duplicate contain all elements ",
		test_cases$integers,
		length(coll) == length(unique(coll)) + length(xDuplicatesOf(coll))
	)

