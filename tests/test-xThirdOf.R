
kiwi ::: load_test_dependencies(environment())

message('xThirdOf')

	over(coll) +

	describe('always returns the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 3,
		xThirdOf(coll) %is% coll[[3]]
	) +
	run()

message('xThirdOf')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) < 3,
		xThirdOf(coll)
	) +

	run()
