
kea ::: load_test_dependencies(environment())

unit_test("xTakeWhile")

	over(coll) +

	it("returns the collection given a truth predicate") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xTakeWhile(function (x) True,  coll) %is% as.list(coll)
	) +

	it("returns the empty list otherwise") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xTakeWhile(function (x) False, coll) %is% keep_names(list(), coll),
		xTakeWhile(function (x) Na,    coll) %is% keep_names(list(), coll)
	) +

	run()




int_test("xTakeWhile")

	over(coll) +

	it("returns indices with <= length") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xTakeWhile(x. <= xLenOf(coll), xIndicesOf(coll)) %is% as.list(xIndicesOf(coll))
	) +

	run()
