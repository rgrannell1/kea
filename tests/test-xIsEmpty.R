
kea ::: load_test_dependencies(environment())

unit_test("xIsEmpty")

	over(coll) +

	it('xIsEmpty correctly reports lengths') +
	holdsWhen(
		suchThat $ is_collection(coll),

		if (length(coll) == 0) xIsEmpty(coll) else !xIsEmpty(coll)
	) +

	run()

int_test("xIsEmpty")

	over(coll) +

	it('is always true with xUnit') +
	holdsWhen(
		suchThat $ is_collection(coll),

		xIsEmpty(xUnit(coll))
	) +


	it('is always true with xSelect(Falsity) and its mirror') +
	holdsWhen(
		suchThat $ is_collection(coll),

		xIsEmpty(xSelect(xFalsity, coll)),
		xIsEmpty(xReject(xTruth,   coll))
	) +


	run()
