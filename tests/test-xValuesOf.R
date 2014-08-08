
kea ::: load_test_dependencies(environment())

message("xValuesOf")

	over(coll) +

	describe("valuesof coll is identity") +
	holdsWhen(
		is_collection(coll) && !is_named(coll),
		xValuesOf(coll) %is% as.list(coll)
	) +

	describe("valuesof coll removes names (named)") +
	holdsWhen(
		is_collection(coll) && is_named(coll),
		xValuesOf(coll) %is% as.list(unname(coll))
	) +

	run()
