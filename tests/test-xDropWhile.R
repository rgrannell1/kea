
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xDropWhile (+)")

	over(coll) +

	describe("true predicates returns the collection") +
	when(
		is_collection(coll),
		xDropWhile(function (x) True,  coll) %equals% list()
	) +

	describe("non true predicates return empty list") +
	when(
		is_collection(coll),
		xDropWhile(function (x) False, coll) %equals% as.list(coll),
		xDropWhile(function (x) Na,    coll) %equals% as.list(coll)
	) +

	run()
