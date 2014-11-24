
kea ::: load_test_dependencies(environment())

message("xTakeWhile")

	over(coll) +

	describe("true predicates returns the collection") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xTakeWhile(function (x) True,  coll) %is% as.list(coll)
	) +

	describe("non true predicates return empty list") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xTakeWhile(function (x) False, coll) %is% list(),
		xTakeWhile(function (x) Na,    coll) %is% list()
	) +

	describe("non true predicates return empty list") +
	holdsWhen(
		suchThat $ is_named_collection(coll),

		xTakeWhile(function (x) False, coll) %is% as_named(list()),
		xTakeWhile(function (x) Na,    coll) %is% as_named(list())
	) +

	run()
