
kea ::: load_test_dependencies()

message("xIndicesOf")

	over(coll) +

	describe("the empty collection always yields the empty integer vector") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xIndicesOf(coll) %is% integer(0)
	) +

	describe("the upper index is the length of the collection") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		max(xIndicesOf(coll)) == length(coll)
	) +

	run()
