
kiwi ::: load_test_dependencies(environment())

message("xTakeWhile")

	over(coll) +

	describe("true predicates returns the collection") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xTakeWhile(function (x) True,  coll) %is% as.list(coll)
	) +

	describe("non true predicates return empty list") +
	holdsWhen(
		is_collection(coll) && !is.named(coll),
		xTakeWhile(function (x) False, coll) %is% list(),
		xTakeWhile(function (x) Na,    coll) %is% list()
	) +

	describe("non true predicates return empty list") +
	holdsWhen(
		is_collection(coll) && is.named(coll),
		xTakeWhile(function (x) False, coll) %is% as_named(list()),
		xTakeWhile(function (x) Na,    coll) %is% as_named(list())
	) +

	run()
