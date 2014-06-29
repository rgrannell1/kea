
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChop (+)")

	over(coll) +

	describe("xChopping infinite times / length times is as.list") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		length(xChop(Inf,          coll)) == length(coll),
		length(xChop(length(coll), coll)) == length(coll)
	) +

	describe("xChop once is almost identity") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xChop(1, coll) %is% list(as.list(coll))
	) +

	run()
