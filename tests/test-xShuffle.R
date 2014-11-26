
kea ::: load_test_dependencies(environment())

message("xShuffle")

	over(coll) +

	it("shuffling the empty collection returns the empty list") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xShuffle(coll) %is% list()
	) +

	it("shuffling the empty collection returns the empty list (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xShuffle(coll) %is% as_named(list())
	) +

	it("length is preserved") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xShuffle(coll)) %is% length(coll)
	) +

	run()
