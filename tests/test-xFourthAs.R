
kiwi ::: load_test_dependencies(environment())


message('xFourthAs')

	over(val, coll) +

	describe('always sets the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 4,
		xFourthAs(val, coll)[[4]] %is% val
	) +

	run()

message('xFourthAs')

	over(val, coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) < 4,
		xFourthAs(val, coll)
	) +

	run()
