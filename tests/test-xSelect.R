
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xSelect (+)")

	over(coll) +

	describe("the empty collection always yields the list") +
	when(
		length(coll) == 0 && is_collection(coll),
		xSelect(function (x) True, coll)  %is% list(),
		xSelect(function (x) False, coll) %is% list(),
		xSelect(function (x) Na,    coll) %is% list()
	) +

	over(coll) +
	describe("truth function acts as identity") +
	when(
		length(coll) > 0 && is_collection(coll),
		xSelect(function (x) True, coll) %is% as.list(coll)
	) +

	over(coll) +
	describe("false or na function acts as unit") +
	when(
		length(coll) > 0 && is_collection(coll),
		xSelect(function (x) False, coll) %is% list(),
		xSelect(function (x) Na,    coll) %is% list()
	) +

	run()
