
kiwi ::: load_test_dependencies(environment())

message('xUniqueOf')

	over(coll) +

	describe('unique of empty coll is empty list') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xUniqueOf(coll) %is% list()
	) +

	describe('uniques + duplicates == set') +
	holdsWhen(
		is_collection(coll),
		length(coll) == length(xUniqueOf(coll)) + length( which(duplicated(coll)) )
	) +

	run()
