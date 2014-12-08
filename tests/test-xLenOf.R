
kea ::: load_test_dependencies(environment())

unit_test("xLenOf")

	over(coll) +

	it('yields the same result as xLenOf') +
	holdsWhen(
		suchThat $ is_collection(coll),

		xLenOf(coll) == length(coll)
	) +

	run()
