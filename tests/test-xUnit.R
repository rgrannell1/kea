
kea ::: load_test_dependencies(environment())

message("xUnit")

	over(coll) +

	describe("xUnit is length-zero") +
	holdsWhen(
		is_collection(coll),
		length(xUnit(coll)) == 0
	) +

	describe("xUnit preserves type") +
	holdsWhen(
		is_collection(coll),
		typeof(xUnit(coll)) == typeof(coll)
	) +

	describe("xUnit of pairlist is null") +
	holdsWhen(
		is.pairlist(coll) || is.null(coll),
		is.null(xUnit(coll))
	) +

	describe("xUnit . xUnit is xUnit") +
	holdsWhen(
		is_collection(coll),

		xUnit(xUnit(coll)) %is% xUnit(coll)
	) +

	run()
