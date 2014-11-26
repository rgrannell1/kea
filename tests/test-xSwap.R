
kea ::: load_test_dependencies(environment())

unit_test("xSwap")

	over(coll, val1, val2) +

	it("swapping a value with itself is identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSwap(val1, val1, coll) %is% as.list(coll)
	) +

	it("replacing a repeated array is repeated replacement value.") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSwap(val1, val2, lapply(coll, function (x) val1)) %is% lapply(coll, function (x) val2)
	) +

	it("swap is idempotent") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSwap(val1, val2, xSwap(val1, val2, coll)) %is% xSwap(val1, val2, coll)
	) +

	run()
