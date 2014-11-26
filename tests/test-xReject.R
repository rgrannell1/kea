
kea ::: load_test_dependencies(environment())

unit_test("xReject")

	over(coll) +

	it("the empty collection always yields the list") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) && !is_named(coll),

		xReject(function (x) True, coll)  %is% list(),
		xReject(function (x) False, coll) %is% list(),
		xReject(function (x) Na,    coll) %is% list()
	) +


	it("the empty collection always yields the list (named)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) && is_named(coll),

		xReject(function (x) True, coll)  %is% as_named(list()),
		xReject(function (x) False, coll) %is% as_named(list()),
		xReject(function (x) Na,    coll) %is% as_named(list())
	) +


	it("truth function acts as identity") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) && !is_named(coll),

		xReject(function (x) True, coll) %is% list()
	) +

	it("truth function acts as identity") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) && is_named(coll),

		xReject(function (x) True, coll) %is% as_named(list())
	) +

	it("false or na function acts as unit") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xReject(function (x) False, coll) %is% as.list(coll),
		xReject(function (x) Na,    coll) %is% as.list(coll)
	) +

	run()

message("xReject")

	over(fn, coll) +

	it("coll must always be a collection") +
	failsWhen(
		suchThat $ not_collection(coll),

		xReject(identity, coll)
	) +

	run()
