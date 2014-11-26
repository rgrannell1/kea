
kea ::: load_test_dependencies(environment())

unit_test("xIndicesOf")

	over(coll) +

	it("the empty collection always yields the empty integer vector") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xIndicesOf(coll) %is% integer(0)
	) +

	it("the upper index is the length of the collection") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		max(xIndicesOf(coll)) == length(coll)
	) +

	run()
