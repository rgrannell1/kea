
kea ::: load_test_dependencies(environment())

message("xSortBy")

	over(coll) +

	it("sorting the empty colelction is the empty list.") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xSortBy(xI, coll) %is% list()
	) +

	it("sorting the empty colelction is the empty list. (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xSortBy(xI, coll) %is% as_named(list())
	) +

	it("sorting the empty colelction is the empty list.") +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) == 1,

		xSortBy(xI, coll) %is% as.list(coll)
	) +

	it("sorting the seq along is identity.") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSortBy(xI, seq_along(coll)) %is% as.list(seq_along(coll))
	) +

	run()
