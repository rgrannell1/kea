
kea ::: load_test_dependencies(environment())

message("xInitOf")

	over(coll) +

	describe("xInitOf of an empty collection yields the empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),

		xInitOf(coll) %is% list()
	) +

	describe("xInitOf of an empty collection yields the empty list (named)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),

		xInitOf(coll) %is% as_named(list())
	) +

	describe("xInitOf of an empty collection yields the empty list") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		length(xInitOf(coll)) == length(coll) - 1
	) +

	run()
