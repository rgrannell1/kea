
kea ::: load_test_dependencies(environment())

message("xExclude")

	over(coll1, coll2) +

	it("excluding right empty list is empty list") +
	holdsWhen(
		suchThat $ is_collection(coll1) && suchThat $ is_empty_collection(coll2),

		xExclude(coll1, coll2) %is% keep_names(list(), coll2)
	) +

	it("excluding the same collection is empty list") +
	holdsWhen(
		suchThat $ is_collection(coll1),

		xExclude(coll1, coll1) %is% keep_names(list(), coll1)
	) +

	it("excluding no elements is identity") +
	holdsWhen(
		suchThat $ is_empty_collection(coll1) && suchThat $ is_collection(coll2),

		xExclude(coll1, coll2) %is% as.list(coll2)
	) +

	it("excluding is idempotent") +
	holdsWhen(
		suchThat $ is_empty_collection(coll1) && suchThat $ is_collection(coll2),

		xExclude(coll1, xExclude(coll1, coll2)) %is% xExclude(coll1, coll2)
	) +

	run()
