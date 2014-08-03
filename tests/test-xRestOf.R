
kea ::: load_test_dependencies(environment())

message("xRestOf")

	over(coll) +

	describe("xRestOf of an empty collection yields the empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		xRestOf(coll) %is% list()
	) +

	describe("xRestOf of an empty collection yields the empty list (named)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		xRestOf(coll) %is% as_named(list())
	) +

	describe("xRestOf of an empty collection yields the empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		length(xRestOf(coll)) == length(coll) - 1
	) +

	run()
