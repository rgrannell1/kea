
kea ::: load_test_dependencies(environment())

unit_test("xUnzipKeys")

	over(coll) +

	it("the empty collection always yields the list") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xUnzipKeys(coll) %is% list()
	) +

	it("returns a 2-tuple") +
	holdsWhen(
		suchThat $ is_named_collection(coll),

		all(vapply(xUnzipKeys(coll), length, integer(1)) == 2)
	) +

	it("coll must always be a collection") +
	failsWhen(
		suchThat $ not_collection(coll),

		xUnzipKeys(coll)
	) +

	run()





int_test("xUnzipKeys")

	over(coll) +

	it('is an inverse of xZipKeys') +
	holdsWhen(
		and_(suchThat $ is_named_collection, suchThat $ not_empty_collection)(coll),

		xZipKeys(xUnzipKeys(coll)) %is% as.list(coll)
	) +

	run()
