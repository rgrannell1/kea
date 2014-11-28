
kea ::: load_test_dependencies(environment())

unit_test("xIndicesOf")

	over(coll) +

	it("keeps the empty input as an empty integer") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xIndicesOf(coll) %is% integer(0)
	) +

	it("keeps the length of its input") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		max(xIndicesOf(coll)) == length(coll)
	) +

	it("is idempotent") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xIndicesOf(xIndicesOf(coll)) %is% xIndicesOf(coll)
	) +

	run()
