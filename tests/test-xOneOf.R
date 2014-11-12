
kea ::: load_test_dependencies()

message("xOneOf")

	over(coll) +

	describe("oneof always selects an element from the list") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xOneOf(coll) %is_in% coll
	) +

	run()

message("xOneOf")

	over(coll) +

	describe("fails when empty collection") +
	failsWhen(
		suchThat $ is_empty_collection(coll),

		xOneOf(coll)
	) +

	run()
