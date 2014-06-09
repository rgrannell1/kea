
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xUniqueOf (+)')

	over(coll) +

	describe('unique of empty coll is empty list') +
	when(
		is_collection(coll) && length(coll) == 0,
		xUniqueOf(coll) %equals% list()
	) +

	describe('uniques + duplicates == set') +
	when(
		is_collection(coll),
		length(coll) == length(xUniqueOf(coll)) + length( which(duplicated(coll)) )
	) +

	run()
