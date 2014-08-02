
kiwi ::: load_test_dependencies(environment())

message("xIsIn")

	over(val, coll) +

	describe("empty set is logical-zero") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xIsIn(val, coll) %is% logical(0)
	) +

	describe("an element in the set is always true") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xIsIn(coll[[ sample.int(length(coll), 1) ]], coll)
	) +

	run()
