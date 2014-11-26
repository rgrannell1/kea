
kea ::: load_test_dependencies(environment())

unit_test("xDropWhile")

	over(coll) +

	it("true predicates returns the collection") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xDropWhile(function (x) True,  coll) %is% list()
	) +

	it("true predicates returns the collection (named)") +
	holdsWhen(
		suchThat $ is_named_collection(coll),

		xDropWhile(function (x) True,  coll) %is% as_named(list())
	) +

	it("non true predicates return empty list") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xDropWhile(function (x) False, coll) %is% as.list(coll),
		xDropWhile(function (x) Na,    coll) %is% as.list(coll)
	) +

	it("dropwhile preserves names") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		names(xDropWhile(function (x) False, coll)) %is% names(coll)
	) +

	run()
