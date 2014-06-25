
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xOneOf (+)")

	over(coll) +

	describe("oneof always selects an element from the list") +
	when(
		length(coll) > 0 && is_collection(coll),
		xOneOf(coll) %in% coll
	) +

	run()

message("xOneOf (-)")

	over(coll) +

	describe("fails when empty collection") +
	failswhen(
		is_collection(coll) && length(coll) == 0,
		xOneOf(coll)
	) +

	run()
