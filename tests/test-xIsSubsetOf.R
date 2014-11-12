
kea ::: load_test_dependencies(environment())

message("xIsSubsetOf")

	over(coll1, coll2) +

	describe("subsetting an empty collection") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) && length(coll1) == 0,

		xIsSubsetOf(coll1, coll2) %is% logical(0)
	) +

	describe("subsetting an empty collection") +
	holdsWhen(
		is_collection(coll1) && is_collection(coll2) && length(coll2) == 0,

		xIsSubsetOf(coll1, coll2) %is% logical(0)
	) +

	describe("elements of a subset are always a subset.") +
	holdsWhen(
		suchThat $ not_empty_collection(coll1),

		{
			subset <- sample(as.list(coll1), size = sample.int(length(coll1), 1))
			xIsSubsetOf(subset, coll1)
		}
	) +

	run()
