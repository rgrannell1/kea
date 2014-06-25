
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChoose (+)")

	over(coll) +

	describe("choosing with empty collection is empty collection") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xChoose(0, list()) %is% list(),
		xChoose(1, list()) %is% list(),
		xChoose(2, list()) %is% list()
	) +

	run()

	over(coll) +

	describe("choosing with 1 is as.list") +
	holdsWhen(
		is_collection(coll),
		xChoose(1, coll) %is% as.list(coll)
	) +

	run()
