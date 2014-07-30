
kiwi ::: load_test_dependencies(environment())

message("xLocate")

	over(coll) +

	describe("the empty collection yields integer(0)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xLocate(function (x) True,  coll) %is% integer(0),
		xLocate(function (x) False, coll) %is% integer(0),
		xLocate(function (x) Na,    coll) %is% integer(0)
	) +

	describe("non-truth functions yield integer(0)") +
	holdsWhen(
		is_collection(coll),
		xLocate(function (x) False, coll) %is% integer(0),
		xLocate(function (x)    Na, coll) %is% integer(0)
	) +

	describe("truth function yields seq_along coll") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xLocate(function (x) True, coll) %is% seq_along(coll)
	) +

	run()

message("xLocate")
