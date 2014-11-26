
kea ::: load_test_dependencies(environment())

unit_test("xPowerSetOf")

	over(coll) +

	it("powerset of empty set is list()") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xPowerSetOf(coll) %is% list()
	) +

	it("powerset has length 2^n") +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) <= 12 && length(coll) > 0,

		length(xPowerSetOf(coll)) == 2^length(coll)
	) +

	run()
