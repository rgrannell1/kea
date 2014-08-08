
kea ::: load_test_dependencies(environment())

message("xSumBy")

over(coll) +

	describe("sum of empty coll is numeric(0)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xSumBy(xI, coll) %is% numeric(0)
	) +

	describe("sum of constant one is length") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xSumBy(xK(1), coll) == length(coll)
	) +

	run()
