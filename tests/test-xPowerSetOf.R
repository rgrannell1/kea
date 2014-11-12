
kea ::: load_test_dependencies(environment())

message("xPowerSetOf")

	over(coll) +

	describe("powerset of empty set is list()") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xPowerSetOf(coll) %is% list()
	) +

	describe("powerset has length 2^n") +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) <= 12 && length(coll) > 0,

		length(xPowerSetOf(coll)) == 2^length(coll)
	) +

	run()
