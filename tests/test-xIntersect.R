
kea ::: load_test_dependencies(environment())

message("xIntersect")

	over(coll1, coll2) +

	describe("interof two empty collection is empty") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) &&
		length(coll1) == 0 && length(coll2) == 0,

		xIntersect(coll1, coll2) %is% list()
	) +

	run()
