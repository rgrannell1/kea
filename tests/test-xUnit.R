
kea ::: load_test_dependencies(environment())

unit_test("xUnit")

	over(coll) +

	it("always returns a length-zero value") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xUnit(coll)) == 0
	) +

	it("preserve its input type") +
	holdsWhen(
		suchThat $ is_collection(coll),

		typeof(xUnit(coll)) == typeof(coll)
	) +

	it("returns null for a pairlist") +
	holdsWhen(
		suchThat $ is_pairlist(coll),

		is.null(xUnit(coll))
	) +

	it("is idempotent") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xUnit(xUnit(coll)) %is% xUnit(coll)
	) +

	run()
