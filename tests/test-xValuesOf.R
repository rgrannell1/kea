
kea ::: load_test_dependencies()

message("xValuesOf")

	over(coll) +

	describe("valuesof coll is identity") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xValuesOf(coll) %is% as.list(coll)
	) +

	describe("valuesof coll removes names (named)") +
	holdsWhen(
		suchThat $ is_named_collection(coll),

		xValuesOf(coll) %is% as.list(unname(coll))
	) +

	run()
