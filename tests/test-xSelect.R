
kiwi ::: load_test_dependencies(environment())

message("xSelect")

	over(coll) +

	describe("the empty collection always yields the list") +
	holdsWhen(
		length(coll) == 0 && is_collection(coll) && !is_named(coll),
		xSelect(function (x) True, coll)  %is% list(),
		xSelect(function (x) False, coll) %is% list(),
		xSelect(function (x) Na,    coll) %is% list()
	) +

	describe("the empty collection always yields the list (named)") +
	holdsWhen(
		length(coll) == 0 && is_collection(coll) && is_named(coll),
		xSelect(function (x) True, coll)  %is% as_named(list()),
		xSelect(function (x) False, coll) %is% as_named(list()),
		xSelect(function (x) Na,    coll) %is% as_named(list())
	) +

	describe("truth function acts as identity") +
	holdsWhen(
		length(coll) > 0 && is_collection(coll),
		xSelect(function (x) True, coll) %is% as.list(coll)
	) +

	describe("false or na function acts as unit") +
	holdsWhen(
		length(coll) > 0 && is_collection(coll) && !is_named(coll),
		xSelect(function (x) False, coll) %is% list(),
		xSelect(function (x) Na,    coll) %is% list()
	) +

	describe("false or na function acts as unit (named)") +
	holdsWhen(
		length(coll) > 0 && is_collection(coll) && is_named(coll),
		xSelect(function (x) False, coll) %is% as_named(list()),
		xSelect(function (x) Na,    coll) %is% as_named(list())
	) +

	run()
