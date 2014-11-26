
kea ::: load_test_dependencies(environment())

message("xInitOf")

	over(coll) +

	it("xInitOf of an empty collection yields the empty list") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xInitOf(coll) %is% list()
	) +

	it("xInitOf of an empty collection yields the empty list (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xInitOf(coll) %is% as_named(list())
	) +

	it("xInitOf of an empty collection yields the empty list") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		length(xInitOf(coll)) == length(coll) - 1
	) +

	it("xInitOf preserves names") +
	holdsWhen(
		suchThat $ is_collection(coll),

		names(xInitOf(coll)) %is% head(names(coll), -1)
	) +

	run()
