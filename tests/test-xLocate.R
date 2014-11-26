
kea ::: load_test_dependencies(environment())

unit_test("xLocate")

	over(coll) +

	it("the empty collection yields integer(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xLocate(function (x) True,  coll) %is% integer(0),
		xLocate(function (x) False, coll) %is% integer(0),
		xLocate(function (x) Na,    coll) %is% integer(0)
	) +

	it("non-truth functions yield integer(0)") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xLocate(function (x) False, coll) %is% integer(0),
		xLocate(function (x)    Na, coll) %is% integer(0)
	) +

	it("truth function yields seq_along coll") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xLocate(function (x) True, coll) %is% seq_along(coll)
	) +

	run()

message("xLocate")
