
kea ::: load_test_dependencies(environment())

message("xDuplicatesOf")

	over(coll) +

	describe("duplications of empty list is empty list") +
	holdsWhen(
		suchThat $ is_empty_collection(coll)

		xDuplicatesOf(coll) %is% list()
	) +

	describe("set = duplicates and unique") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(coll) == length(unique(coll)) + length(xDuplicatesOf(coll))
	) +

	run()
