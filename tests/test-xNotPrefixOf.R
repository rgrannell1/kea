
kea ::: load_test_dependencies(environment())

message("xNotPrefix")

	over(coll1, coll2) +

	describe("a collection is a prefix of itself.") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) &&
		(length(coll1) == 0 || length(coll2) == 0),

		xNotPrefix(coll1, coll2) %is% logical(0)
	) +

	describe("a collection is a prefix of itself.") +
	holdsWhen(
		is_collection(coll1) && length(coll1) > 0,

		!xNotPrefix(coll1, coll1)
	) +

	describe("continuous subsequences are always members.") +
	holdsWhen(
		is_collection(coll1) && length(coll1) > 0,

		!xNotPrefix(
			head(coll1, max(c(1, sample.int(length(coll1), 1)) )),
			coll1)
	) +

	run()