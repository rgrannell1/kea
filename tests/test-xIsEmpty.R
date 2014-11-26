
kea ::: load_test_dependencies(environment())

unit_test("xIsEmpty")

	over(coll) +

	it('xIsEmpty correctly reports lengths.') +
	holdsWhen(
		suchThat $ is_collection(coll),

		if (length(coll) == 0) xIsEmpty(coll) else !xIsEmpty(coll)
	) +

	run()
