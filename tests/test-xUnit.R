
kiwi ::: load_test_dependencies(environment())

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

	run()

message("xUnit")

	over(fn, coll) +

	describe("coll must always be a collection") +
	failsWhen(
		!is_collection(coll),
		xUnit(coll)
	) +

	run()
