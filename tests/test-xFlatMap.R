
kiwi ::: load_test_dependencies(environment())

message("xFlatMap")

	over(fn, coll) +

	describe("flatmap of empty collection is always empty") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		xFlatMap(identity, coll) %is% list()
	) +

	describe("flatmap of empty collection is always empty (named)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		xFlatMap(identity, coll) %is% as_named(list())
	) +

	describe("flatmap with identity is the coll") +
	holdsWhen(
		is_collection(coll),
		xFlatMap(identity, coll) %is% as.list(coll)
	) +

	run()
