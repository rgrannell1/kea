
kea ::: load_test_dependencies(environment())

message("xNotSubsetOf")

	over(coll1, coll2) +

	describe("subsetting an empty collection") +
	holdsWhen(
		suchThat $ is_empty_collection(coll1) && suchThat $ is_collection(coll2),

		xNotSubsetOf(coll1, coll2) %is% logical(0)
	) +

	describe("subsetting an empty collection") +
	holdsWhen(
		suchThat $ is_collection(coll1) && suchThat $ is_empty_collection(coll2),

		xNotSubsetOf(coll1, coll2) %is% logical(0)
	) +

	describe("elements of a subset are always a subset.") +
	holdsWhen(
		suchThat $ not_empty_collection(coll1),

		{
			subset <- sample(as.list(coll1), size = sample.int(length(coll1), 1))
			!xNotSubsetOf(subset, coll1)
		}
	) +

	run()
