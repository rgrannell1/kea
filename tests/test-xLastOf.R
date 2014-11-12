
kea ::: load_test_dependencies(environment())

message('xLastOf')

	over(coll) +

	describe('always returns the correct element') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xLastOf(coll) %is% coll[[ length(coll) ]]
	) +

	run()

message('xLastOf')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		suchThat $ is_empty_collection(coll),

		xLastOf(coll)
	) +

	run()
