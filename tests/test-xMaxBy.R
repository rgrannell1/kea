
kea ::: load_test_dependencies(environment())

unit_test("xMaxBy")

	over(coll) +

	it("fails for empty collections") +
	failsWhen(
		suchThat $ is_empty_collection(coll),

		xMaxBy(identity, coll)
	) +

	it("is identity for single values") +
	holdsWhen(
		and_(suchThat $ is_collection, suchThat $ is_singleton)(coll),

		xMaxBy(identity, coll) %is% coll[[1]]
	) +

	run()

	over(num, coll) +

	it("returns the first element given a constant metric") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) &&
		is.numeric(num) && length(num) == 1 &&
		!is.na(num),

		xMaxBy(xCapture(num), coll) %is% coll[[1]]
	) +

	run()
