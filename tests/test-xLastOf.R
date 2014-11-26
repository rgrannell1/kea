
kea ::: load_test_dependencies(environment())

unit_test('xLastOf')

	over(coll) +

	it('always returns the correct element') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xLastOf(coll) %is% coll[[ length(coll) ]]
	) +

	run()

message('xLastOf')

	over(coll) +

	it('fails when the collection is too short') +
	failsWhen(
		suchThat $ is_empty_collection(coll),

		xLastOf(coll)
	) +

	run()
