
kiwi ::: load_test_dependencies(environment())

message("xPowerSetOf")

	over(coll) +

	describe("powerset of empty set is list()") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xPowerSetOf(coll) %is% list()
	) +

	describe("powerset has length 2^n") +
	holdsWhen(
		is_collection(coll) && length(coll) <= 12 && length(coll) > 0,
		length(xPowerSetOf(coll)) == 2^length(coll)
	) +

	run()
