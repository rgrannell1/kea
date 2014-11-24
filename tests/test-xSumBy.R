
kea ::: load_test_dependencies(environment())

message("xSumBy")

over(coll) +

	describe("sum of empty coll is numeric(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xSumBy(xI, coll) %is% numeric(0)
	) +

	describe("sum of constant one is length") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xSumBy(xK(1), coll) == length(coll)
	) +

	run()
