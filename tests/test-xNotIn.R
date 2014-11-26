
kea ::: load_test_dependencies(environment())

message("xNotIn")

	over(val, coll) +

	it("empty set is logical-zero") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xNotIn(val, coll) %is% logical(0)
	) +

	it("an element in the set is not true") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		!xNotIn(coll[[ sample.int(length(coll), 1) ]], coll)
	) +

	run()
