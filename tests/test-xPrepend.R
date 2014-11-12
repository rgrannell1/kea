
kea ::: load_test_dependencies(environment())

message("xPrepend")

	over(val, coll) +

	describe("joining with empty collection is list(val)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xPrepend(val, coll) %is% list(val)
	) +

	describe("adds to front of collection") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xPrepend(val, coll)) == length(coll) + 1,
		xPrepend(val, coll)[[1]] %is% val
	) +

	run()
