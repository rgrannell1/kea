
kea ::: load_test_dependencies(environment())

message('xLastOf')

	over(coll) +

	describe('always returns the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 1,
		xLastOf(coll) %is% coll[[ length(coll) ]]
	) +

	run()

message('xLastOf')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) == 0,
		xLastOf(coll)
	) +

	run()
