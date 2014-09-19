
kea ::: load_test_dependencies(environment())

message("xNotSubsetOf")

	over(coll1, coll2) +

	describe("subsetting an empty collection") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) &&
		length(coll1) == 0,
		xNotSubsetOf(coll1, coll2) %is% logical(0)
	) +

	describe("subsetting an empty collection") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) &&
		length(coll2) == 0,
		xNotSubsetOf(coll1, coll2) %is% logical(0)
	) +

	describe("elements of a subset are always a subset.") +
	holdsWhen(
		is_collection(coll1) && length(coll1) > 0,
		{
			subset <- sample(as.list(coll1), size = sample.int(length(coll1), 1))
			!xNotSubsetOf(subset, coll1)
		}
	) +

	run()
