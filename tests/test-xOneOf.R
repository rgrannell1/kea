
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message("xOneOf (+)")

	over(coll) +
	describe("oneof always selects an element from the list") +
	when(
		length(coll) > 0 && is_collection(coll),
		xOneOf(coll) %in% coll) +
	run()

message("xOneOf (-)")

	over(coll) +
	describe("xOneOf fails if the collection is empty") +
	when(
		length(coll) == 0 && is_collection(coll),
		xOneOf(coll)) +
	run()

	over(coll) +
	describe("xOneOf fails for non-collections") +
	failsWhen(
		!is_collection(coll),
		xOneOf(coll)) +
	run()
