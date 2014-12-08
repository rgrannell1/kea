
kea ::: load_test_dependencies(environment())

unit_test('xInvoke')

	over(coll) +

	it("preserves inputs") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xInvoke(identity, coll) %is% coll
	) +

	run()
