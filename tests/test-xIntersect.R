
kea ::: load_test_dependencies(environment())

message("xIntersect")

	over(coll1, coll2) +

	describe("interof two empty collection is empty") +
	holdsWhen(
		suchThat $ is_empty_collection(coll1) && suchThat $ is_empty_collection(coll2),

		xIntersect(coll1, coll2) %is% list()
	) +

	run()
