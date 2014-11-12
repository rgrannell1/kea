
kea ::: load_test_dependencies(environment())

message('xDistinctOf')

	over(coll) +

	describe('unique of empty coll is empty list') +
	holdsWhen(
		suchThat $ is_empty_collection(coll)

		xDistinctOf(coll) %is% list()
	) +

	describe('uniques + duplicates == set') +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(coll) == length(xDistinctOf(coll)) + length( which(duplicated(coll)) )
	) +

	describe('unique repeats')+
	holdsWhen(
		suchThat $ is_collection(coll),

		xDistinctOf(xDistinctOf(coll)) %is% xDistinctOf(coll)
	) +

	run()
