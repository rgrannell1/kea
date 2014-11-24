
kea ::: load_test_dependencies(environment())

message("xExclude")

	over(coll1, coll2) +

	describe("excluding right empty list is empty list") +
	holdsWhen(
		suchThat $ is_collection(coll1) && suchThat $ is_empty_collection(coll2),

		xExclude(coll1, coll2) %is% keep_names(list(), coll2)
	) +

	describe("excluding the same collection is empty list") +
	holdsWhen(
		suchThat $ is_collection(coll1),

		xExclude(coll1, coll1) %is% keep_names(list(), coll1)
	) +

	describe("excluding no elements is identity") +
	holdsWhen(
		suchThat $ is_empty_collection(coll1) && suchThat $ is_collection(coll2),

		xExclude(coll1, coll2) %is% as.list(coll2)
	) +

	run()
