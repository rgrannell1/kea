
kiwi ::: load_test_dependencies(environment())

message("xInterOf")

	over(coll1, coll2) +

	describe("interof two empty collection is empty") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) &&
		length(coll1) == 0 && length(coll2) == 0,

		xInterOf(list(coll1, coll2)) %is% list()
	) +

	run()
