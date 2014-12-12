
kea ::: load_test_dependencies(environment())

unit_test("xClear")

	over(coll) +

	it("always returns a length-zero value") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xClear(coll)) == 0
	) +

	it("preserve its input type") +
	holdsWhen(
		suchThat $ is_collection(coll),

		typeof(xClear(coll)) == typeof(coll)
	) +

	it("returns null for a pairlist") +
	holdsWhen(
		suchThat $ is_pairlist(coll),

		is.null(xClear(coll))
	) +

	it("is idempotent") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xClear(xClear(coll)) %is% xClear(coll)
	) +

	run()
