
kea ::: load_test_dependencies(environment())

unit_test("xProdBy")

over(coll) +

	it("prod of empty coll is numeric(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xProdBy(xI, coll) %is% numeric(0)
	) +

	it("prod of constant zero is zero") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xSumBy(xK(0), coll) == 0
	) +

	run()
