
kiwi ::: load_test_dependencies(environment())


message("xDropWhile (+)")

	over(coll) +

	describe("true predicates returns the collection") +
	holdsWhen(
		is_collection(coll),
		xDropWhile(function (x) True,  coll) %is% list()
	) +

	describe("non true predicates return empty list") +
	holdsWhen(
		is_collection(coll),
		xDropWhile(function (x) False, coll) %is% as.list(coll),
		xDropWhile(function (x) Na,    coll) %is% as.list(coll)
	) +

	run()
