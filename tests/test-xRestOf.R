
kea ::: load_test_dependencies(environment())

message("xRestOf")

	over(coll) +

	it("xRestOf of an empty collection yields the empty list") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xRestOf(coll) %is% list()
	) +

	it("xRestOf of an empty collection yields the empty list (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xRestOf(coll) %is% as_named(list())
	) +

	it("xRestOf of an empty collection yields the empty list") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		length(xRestOf(coll)) == length(coll) - 1
	) +

	it("xRestOf preserves names") +
	holdsWhen(
		suchThat $ is_collection(coll),

		names(xRestOf(coll)) %is% tail(names(coll), -1)
	) +

	run()
