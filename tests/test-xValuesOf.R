
kea ::: load_test_dependencies(environment())

message("xValuesOf")

	over(coll) +

	it("valuesof coll is identity") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xValuesOf(coll) %is% as.list(coll)
	) +

	it("valuesof coll removes names (named)") +
	holdsWhen(
		suchThat $ is_named_collection(coll),

		xValuesOf(coll) %is% as.list(unname(coll))
	) +

	run()
