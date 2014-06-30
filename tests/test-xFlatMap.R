
kiwi ::: load_test_dependencies(environment())

message("xFlatMap")

	over(fn, coll) +

	describe("flatmap of empty collection is always empty") +
	holdsWhen(
		is.function(fn) && is_collection(coll) && length(coll) == 0,
		xFlatMap(fn, coll) %is% list()
	) +

	describe("flatmap with identity is the coll") +
	holdsWhen(
		is_collection(coll),
		xFlatMap(identity, coll) %is% as.list(coll)
	) +

	run()
