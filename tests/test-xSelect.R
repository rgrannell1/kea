
kea ::: load_test_dependencies(environment())

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





	describe("one-off equalities") +
	holdsFor(
		xSelect(function (x) x %% 2 == 0, 1:10) %is% list(2L, 4L, 6L, 8L, 10L)
	) +

	describe("one-off failures") +
	failsFor(
		xSelect(function (x) 1, 1:10)
	) +

	run()
