
kiwi ::: load_test_dependencies(environment())

message("xAppend")

	over(val, coll) +

	describe("joining with empty collection is list(val)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xAppend(val, coll) %is% list(val)
	) +

	describe("adds to end of collection") +
	holdsWhen(
		is_collection(coll),
		length(xAppend(val, coll)) == length(coll) + 1,
		xAppend(val, coll)[[length(coll) + 1]] %is% val
	) +

	run()

message("xAppend")

	over(val, coll) +

	describe("fails if not a collection") +
	failsWhen(
		!is_collection(coll),
		xAppend(val, coll)
	) +

	run()
