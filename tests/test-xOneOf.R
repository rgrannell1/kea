
kiwi ::: load_test_dependencies(environment())


message("xOneOf (+)")

	over(coll) +

	describe("oneof always selects an element from the list") +
	holdsWhen(
		length(coll) > 0 && is_collection(coll),
		xOneOf(coll) %in% coll
	) +

	run()

message("xOneOf (-)")

	over(coll) +

	describe("fails when empty collection") +
	failsWhen(
		is_collection(coll) && length(coll) == 0,
		xOneOf(coll)
	) +

	run()
