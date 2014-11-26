
kea ::: load_test_dependencies(environment())

unit_test("xIsIn")

	over(val, coll) +

	it("empty set is logical-zero") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xIsIn(val, coll) %is% logical(0)
	) +

	it("an element in the set is always true") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xIsIn(coll[[ sample.int(length(coll), 1) ]], coll)
	) +

	run()
