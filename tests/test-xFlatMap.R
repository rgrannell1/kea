
kea ::: load_test_dependencies(environment())

unit_test("xFlatMap")

	over(fn, coll) +

	it("flatmap of empty collection is always empty") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xFlatMap(identity, coll) %is% list()
	) +

	it("flatmap of empty collection is always empty (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xFlatMap(identity, coll) %is% as_named(list())
	) +

	it("flatmap with identity is the coll") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xFlatMap(identity, coll) %is% as.list(coll)
	) +

	run()
