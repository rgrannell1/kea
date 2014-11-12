
kea ::: load_test_dependencies(environment())

message("xUnit")

	over(coll) +

	describe("xUnit is length-zero") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xUnit(coll)) == 0
	) +

	describe("xUnit preserves type") +
	holdsWhen(
		suchThat $ is_collection(coll),

		typeof(xUnit(coll)) == typeof(coll)
	) +

	describe("xUnit of pairlist is null") +
	holdsWhen(
		is.pairlist(coll) || is.null(coll),

		is.null(xUnit(coll))
	) +

	describe("xUnit . xUnit is xUnit") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xUnit(xUnit(coll)) %is% xUnit(coll)
	) +

	run()
