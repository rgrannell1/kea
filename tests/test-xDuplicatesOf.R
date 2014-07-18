
kiwi ::: load_test_dependencies(environment())

message("xDuplicatesOf")

	over(coll) +

	describe("duplications of empty list is empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xDuplicatesOf(coll) %is% list()
	) +

	describe("set = duplicates and unique") +
	holdsWhen(
		is_collection(coll),
		length(coll) == length(unique(coll)) + length(xDuplicatesOf(coll))
	) +

	run()
