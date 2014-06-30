
kiwi ::: load_test_dependencies(environment())


message('xThirdAs (+)')

	over(val, coll) +

	describe('always sets the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 3,
		xThirdAs(val, coll)[[3]] %is% val
	) +

	run()

message('xThirdAs (-)')

	over(val, coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) < 3,
		xThirdAs(val, coll)
	) +

	run()
