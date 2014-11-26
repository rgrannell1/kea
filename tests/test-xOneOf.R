
kea ::: load_test_dependencies(environment())

unit_test("xOneOf")

	over(coll) +

	it("oneof always selects an element from the list") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xOneOf(coll) %is_in% coll
	) +

	run()

message("xOneOf")

	over(coll) +

	it("fails when empty collection") +
	failsWhen(
		suchThat $ is_empty_collection(coll),

		xOneOf(coll)
	) +

	run()
