
kea ::: load_test_dependencies(environment())

message("xProdBy")

over(coll) +

	describe("prod of empty coll is numeric(0)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xProdBy(xI, coll) %is% numeric(0)
	) +

	describe("prod of constant zero is zero") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xSumBy(xK(0), coll) == 0
	) +

	run()
