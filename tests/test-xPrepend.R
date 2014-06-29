
kiwi ::: load_test_dependencies(environment())


message("xPrepend (+)")

	over(val, coll) +

	describe("joining with empty collection is list(val)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xPrepend(val, coll) %is% list(val)
	) +

	describe("adds to front of collection") +
	holdsWhen(
		is_collection(coll),
		length(xPrepend(val, coll)) == length(coll) + 1,
		xPrepend(val, coll)[[1]] %is% val
	) +

	run()

message("xPrepend (-)")

	over(val, coll) +

	describe("fails if not a collection") +
	failsWhen(
		!is_collection(coll),
		xPrepend(val, coll)
	) +

	run()
