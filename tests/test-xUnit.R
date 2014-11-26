
kea ::: load_test_dependencies(environment())

unit_test("xUnit")

	over(coll) +

	it("xUnit is length-zero") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xUnit(coll)) == 0
	) +

	it("xUnit preserves type") +
	holdsWhen(
		suchThat $ is_collection(coll),

		typeof(xUnit(coll)) == typeof(coll)
	) +

	it("xUnit of pairlist is null") +
	holdsWhen(
		suchThat $ is_pairlist(coll),

		is.null(xUnit(coll))
	) +

	it("xUnit . xUnit is xUnit") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xUnit(xUnit(coll)) %is% xUnit(coll)
	) +

	run()
