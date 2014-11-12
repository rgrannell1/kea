
kea ::: load_test_dependencies(environment())

message("xFlatMap")

	over(fn, coll) +

	describe("flatmap of empty collection is always empty") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xFlatMap(identity, coll) %is% list()
	) +

	describe("flatmap of empty collection is always empty (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xFlatMap(identity, coll) %is% as_named(list())
	) +

	describe("flatmap with identity is the coll") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xFlatMap(identity, coll) %is% as.list(coll)
	) +

	run()
