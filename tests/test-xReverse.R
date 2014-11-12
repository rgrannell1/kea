
kea ::: load_test_dependencies(environment())

message("xReverse")

	over(coll) +

	describe("reversing the empty list is the empty collection") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xReverse(coll) %is% list()
	) +

	describe("reversing the empty list is the empty collection (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xReverse(coll) %is% as_named(list())
	) +

	describe("reversing a collection is the reversed collection") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xReverse(coll) %is% as.list(rev(coll)),
		xReverse(xReverse(coll)) %is% as.list(coll)
	) +

	describe("names are preserved and reversed") +
	holdsWhen(
		suchThat $ is_collection(coll),

		names(xReverse(coll)) %is% rev(names(coll))
	) +

	run()
