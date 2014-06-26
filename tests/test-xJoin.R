
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xJoin (+)")

	over(coll1, coll2, coll3) +

	describe('a single collection acts as identity') +
	holdsWhen(
		is_collection(coll1),
		xJoin(list(coll1)) %is% as.list(coll1)
	) +

	describe('joining an empty collection with non-empty is the non-empty collection (left)') +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) && length(coll2) == 0,
		xJoin(list(coll1, coll2)) %is% as.list(coll1),
		xJoin(list(coll2, coll1)) %is% as.list(coll1)
	) +

	run()
