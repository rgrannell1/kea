
kea ::: load_test_dependencies(environment())

message("xSelect")

	over(coll) +

	it("the empty collection always yields the list") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xSelect(function (x) True, coll)  %is% list(),
		xSelect(function (x) False, coll) %is% list(),
		xSelect(function (x) Na,    coll) %is% list()
	) +

	it("the empty collection always yields the list (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xSelect(function (x) True, coll)  %is% as_named(list()),
		xSelect(function (x) False, coll) %is% as_named(list()),
		xSelect(function (x) Na,    coll) %is% as_named(list())
	) +

	it("truth function acts as identity") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xSelect(function (x) True, coll) %is% as.list(coll)
	) +

	it("false or na function acts as unit") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) && !is_named(coll),

		xSelect(function (x) False, coll) %is% list(),
		xSelect(function (x) Na,    coll) %is% list()
	) +

	it("false or na function acts as unit (named)") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) && is_named(coll),

		xSelect(function (x) False, coll) %is% as_named(list()),
		xSelect(function (x) Na,    coll) %is% as_named(list())
	) +

	run()
