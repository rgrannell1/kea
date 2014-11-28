
kea ::: load_test_dependencies(environment())

unit_test("xSelect")

	over(coll) +

	it("yields the empty collection for empty lists") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xSelect(function (x) True, coll)  %is% keep_names(list(), coll),
		xSelect(function (x) False, coll) %is% keep_names(list(), coll),
		xSelect(function (x) Na,    coll) %is% keep_names(list(), coll)
	) +

	it("yields a collection with the truth function") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xSelect(function (x) True, coll) %is% as.list(coll)
	) +

	it("yields the empty list for other functions") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) && !is_named(coll),

		xSelect(function (x) False, coll) %is% keep_names(list(), coll),
		xSelect(function (x) Na,    coll) %is% keep_names(list(), coll)
	) +

	run()
