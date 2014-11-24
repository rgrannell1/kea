
kea ::: load_test_dependencies(environment())

message("xIntersect")

	over(coll1, coll2) +

	describe("interof of empty collection is empty (left)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll1) && suchThat $ is_collection(coll2) ,

		xIntersect(coll1, coll2) %is% keep_names(list(), coll2)
	) +

	describe("interof of empty collection is empty (right)") +
	holdsWhen(
		suchThat $ is_collection(coll1) && suchThat $ is_empty_collection(coll2) ,

		xIntersect(coll1, coll2) %is% keep_names(list(), coll2)
	) +

	describe("interof of empty collection is empty (both)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll1) && suchThat $ is_empty_collection(coll2) ,

		xIntersect(coll1, coll2) %is% keep_names(list(), coll2)
	) +

	describe("interof with self is self") +
	holdsWhen(
		suchThat $ is_collection(coll1),

		xIntersect(coll1, coll1) %is% as.list(coll1)
	) +

	run()
