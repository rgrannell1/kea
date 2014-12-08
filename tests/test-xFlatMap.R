
kea ::: load_test_dependencies(environment())

unit_test("xFlatMap")

	over(fn, coll) +

	it("returns empty collections for empty collections") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xFlatMap(identity, coll) %is% keep_names(list(), coll)
	) +

	it("is identity for collections with identity function") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xFlatMap(identity, coll) %is% as.list(coll)
	) +

	run()
