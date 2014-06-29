
kiwi ::: load_test_dependencies(environment())


message("xTakeWhile (+)")

	over(coll) +

	describe("true predicates returns the collection") +
	holdsWhen(
		is_collection(coll),
		xTakeWhile(function (x) True,  coll) %is% as.list(coll)
	) +

	describe("non true predicates return empty list") +
	holdsWhen(
		is_collection(coll),
		xTakeWhile(function (x) False, coll) %is% list(),
		xTakeWhile(function (x) Na,    coll) %is% list()
	) +

	run()
