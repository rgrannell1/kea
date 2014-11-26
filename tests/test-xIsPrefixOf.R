
kea ::: load_test_dependencies(environment())

unit_test("xIsPrefixOf")

	over(coll1, coll2) +

	it("a collection is a prefix of itself.") +
	holdsWhen(
		suchThat $ is_collection(coll1) && suchThat $ is_collection(coll2) &&
		suchThat $ is_empty_collection(coll1) ||
		suchThat $ is_empty_collection(coll2)

		xIsPrefixOf(coll1, coll2) %is% logical(0)
	) +

	it("a collection is a prefix of itself.") +
	holdsWhen(
		suchThat $ not_empty_collection(coll1),

		xIsPrefixOf(coll1, coll1)
	) +

	it("continuous subsequences are always members.") +
	holdsWhen(
		suchThat $ not_empty_collection(coll1),

		xIsPrefixOf(
			head(coll1, max(c(1, sample.int(length(coll1), 1)) )),
			coll1)
	) +

	run()
