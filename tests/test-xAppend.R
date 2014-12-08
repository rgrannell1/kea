
kea ::: load_test_dependencies(environment())

unit_test("xAppend")

	over(val, coll) +

	it("joining with empty collection is list(val)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xAppend(val, coll) %is% list(val)
	) +

	it("adds to end of collection") +
	holdsWhen(
		suchThat $ is_collection(coll),
		length(xAppend(val, coll)) == length(coll) + 1,

		xAppend(val, coll)[[length(coll) + 1]] %is% val
	) +

	run()

message("xAppend")

	over(val, coll) +

	it("fails if not a collection") +
	failsWhen(
		suchThat $ not_collection(coll),

		xAppend(val, coll)
	) +

	run()
