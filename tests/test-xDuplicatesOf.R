
kea ::: load_test_dependencies(environment())

message("xDuplicatesOf")

	over(coll) +

	it("duplications of empty list is empty list") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xDuplicatesOf(coll) %is% list()
	) +

	it("set = duplicates and unique") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(coll) == length(unique(coll)) + length(xDuplicatesOf(coll))
	) +

	run()
