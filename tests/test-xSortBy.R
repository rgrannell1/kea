
kea ::: load_test_dependencies(environment())

message("xSortBy")

	over(coll) +

	describe("sorting the empty colelction is the empty list.") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		xSortBy(xI, coll) %is% list()
	) +

	describe("sorting the empty colelction is the empty list. (named)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		xSortBy(xI, coll) %is% as_named(list())
	) +

	describe("sorting the empty colelction is the empty list.") +
	holdsWhen(
		is_collection(coll) && length(coll) == 1,
		xSortBy(xI, coll) %is% as.list(coll)
	) +

	describe("sorting the seq along is identity.") +
	holdsWhen(
		is_collection(coll),
		xSortBy(xI, seq_along(coll)) %is% as.list(seq_along(coll))
	) +

	run()
