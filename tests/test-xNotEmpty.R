
kea ::: load_test_dependencies(environment())

unit_test("xNotEmpty")

	over(coll) +

	it('xNotEmpty correctly reports lengths.') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		!xNotEmpty(coll)
	) +

	it('xNotEmpty correctly reports lengths.') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xNotEmpty(coll)
	) +


	run()
