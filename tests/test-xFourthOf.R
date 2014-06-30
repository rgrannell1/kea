
kiwi ::: load_test_dependencies(environment())


message('xFourthOf')

	over(coll) +

	describe('always returns the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 4,
		xFourthOf(coll) %is% coll[[4]]
	) +
	run()

message('xFourthOf')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) < 4,
		xFourthOf(coll)
	) +

	run()
