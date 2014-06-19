
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChop (+)")

	over(coll) +

	describe("xChopping infinite times / length times is as.list") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChop(Inf,          coll) %is% lapply(coll, list),
		xChop(length(coll), coll) %is% lapply(coll, list)
	) +

	describe("xChop once is almost identity") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChop(1, coll) %is% list(as.list(coll))
	) +

	run()
