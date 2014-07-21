
kiwi ::: load_test_dependencies(environment())

message("xNotIn")

	over(val, coll) +

	describe("empty set is logical-zero") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xNotIn(val, coll) %is% logical(0)
	) +

	describe("an element in the set is not true") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		!xNotIn(coll[[ sample.int(length(coll), 1) ]], coll)
	) +

	run()
