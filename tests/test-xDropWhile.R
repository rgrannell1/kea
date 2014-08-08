
kea ::: load_test_dependencies(environment())

message("xDropWhile")

	over(coll) +

	describe("true predicates returns the collection") +
	holdsWhen(
		is_collection(coll) && !is_named(coll),

		xDropWhile(function (x) True,  coll) %is% list()
	) +

	describe("true predicates returns the collection (named)") +
	holdsWhen(
		is_collection(coll) && is_named(coll),

		xDropWhile(function (x) True,  coll) %is% as_named(list())
	) +

	describe("non true predicates return empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xDropWhile(function (x) False, coll) %is% as.list(coll),
		xDropWhile(function (x) Na,    coll) %is% as.list(coll)
	) +

	run()
