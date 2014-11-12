
kea ::: load_test_dependencies(environment())

message("xJoin")

	over(coll1, coll2) +

	describe('empty coll is unnamed list') +
	holdsWhen(
		is_collection(coll1) && length(coll1) == 0,

		xJoin(list()) %is% list(),
		xJoin_(coll1) %is% list()
	) +

	describe('a single collection acts as identity') +
	holdsWhen(
		suchThat $ not_empty_collection(coll1),

		xJoin(list(coll1)) %is% as.list(coll1)
	) +

	describe('joining an empty collection with non-empty is the non-empty collection (left)') +
	holdsWhen(
		suchThat $ is_collection(coll1) && suchThat $ is_collection(coll2),

		length(xJoin_(coll1, coll2)) == length(coll1) + length(coll2)
	) +

	run()
