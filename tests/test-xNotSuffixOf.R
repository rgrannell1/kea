
kea ::: load_test_dependencies(environment())

unit_test("xNotSuffixOf")

	over(coll1, coll2) +

	it("a collection is a prefix of itself.") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) && (length(coll1) == 0 || length(coll2) == 0),

		xNotSuffixOf(coll1, coll2) %is% logical(0)
	) +

	it("a collection is a prefix of itself.") +
	holdsWhen(
		suchThat $ not_empty_collection(coll1),

		!xNotSuffixOf(coll1, coll1)
	) +

	run()
