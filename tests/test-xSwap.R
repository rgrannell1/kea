
kea ::: load_test_dependencies(environment())

message("xSwap")

	over(coll, val) +

	describe("swapping a value with itself is identity") +
	holdsWhen(
		is_collection(coll),
		xSwap(val, val, coll) %is% as.list(coll)
	) +

	run()
