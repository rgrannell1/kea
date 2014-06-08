
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xLocate (+)")

	over(coll) +

	describe("the empty collection yields integer(0)") +
	when(
		is_collection(coll) && length(coll) == 0,
		xLocate(function (x) True,  coll) %equals% integer(0),
		xLocate(function (x) False, coll) %equals% integer(0),
		xLocate(function (x) Na,    coll) %equals% integer(0)
	) +

	describe("non-truth functions yield integer(0)") +
	when(
		is_collection(coll),
		xLocate(function (x) False, coll) %equals% integer(0),
		xLocate(function (x)    Na, coll) %equals% integer(0)
	) +

	describe("truth function yields seq_along coll") +
	when(
		is_collection(coll) && length(coll) > 0,
		xLocate(function (x) True, coll) %equals% seq_along(coll)
	) +

	run()

message("xLocate (-)")
