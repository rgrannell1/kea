
kea ::: load_test_dependencies(environment())

message('xDistinctOf')

	over(coll) +

	describe('unique of empty coll is empty list') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xDistinctOf(coll) %is% list()
	) +

	describe('uniques + duplicates == set') +
	holdsWhen(
		is_collection(coll),

		length(coll) == length(xDistinctOf(coll)) + length( which(duplicated(coll)) )
	) +

	describe('unique repeats')+
	holdsWhen(
		is_collection(coll),

		xDistinctOf(xDistinctOf(coll)) %is% xDistinctOf(coll)
	) +

	run()
