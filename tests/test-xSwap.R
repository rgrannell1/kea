
kea ::: load_test_dependencies(environment())

message("xSwap")

	over(coll, val1, val2) +

	describe("swapping a value with itself is identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSwap(val1, val1, coll) %is% as.list(coll)
	) +

	describe("replacing a repeated array is repeated replacement value.") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSwap(val1, val2, lapply(coll, function (x) val1)) %is% lapply(coll, function (x) val2)
	) +

	run()
