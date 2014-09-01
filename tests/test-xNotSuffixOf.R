
kea ::: load_test_dependencies(environment())

message("xNotSuffixOf")

	over(coll1, coll2) +

	describe("a collection is a prefix of itself.") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) && (length(coll1) == 0 || length(coll2) == 0),
		xNotSuffixOf(coll1, coll2) %is% logical(0)
	) +

	describe("a collection is a prefix of itself.") +
	holdsWhen(
		is_collection(coll1) && length(coll1) > 0,
		!xNotSuffixOf(coll1, coll1)
	) +

	describe("continuous subsequences are always members.") +
	holdsWhen(
		is_collection(coll1) && length(coll1) > 0,
		{
			ith <- sample.int(length(coll1), 1)

			if (length(coll1) == 1 && ith == 1) {
				ith <- -1
			}

			!xNotSuffixOf(head(coll1, -ith), coll1)
		}
	) +

	run()