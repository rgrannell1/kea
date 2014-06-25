
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xUnit (+)")

	over(coll) +

	describe("xUnit is length-zero") +
	when(
		is_collection(coll),
		length(xUnit(coll)) == 0
	) +

	describe("xUnit preserves type") +
	when(
		is_collection(coll),
		typeof(xUnit(coll)) == typeof(coll)
	) +

	describe("xUnit of pairlist is null") +
	when(
		is.pairlist(coll) || is.null(coll),
		is.null(xUnit(coll))
	) +

	run()

message("xUnit (-)")

	over(fn, coll) +

	describe("coll must always be a collection") +
	failswhen(
		!is_collection(coll),
		xUnit(coll)
	) +

	run()
