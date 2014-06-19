
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xRankOf (+)")

	over(coll) +

	describe("rank of empty collection is integer(0)") +
	when(
		is_collection(coll) && length(coll) == 0,
		xRankOf(coll) %is% integer(0)
	) +

	describe("the rank of one number is one.") +
	when(
		is_numeric(coll) && length(coll) == 1,
		xRankOf(coll) == 1
	) +

	run()
