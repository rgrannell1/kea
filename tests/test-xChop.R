
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChop (+)")

	over(coll) +

	describe("xChop once is almost identity") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xChop(1, coll) %is% list(as.list(coll))
	) +

	run()
