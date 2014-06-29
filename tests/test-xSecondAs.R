
kiwi ::: load_test_dependencies(environment())


message('xSecondAs (+)')

	over(val, coll) +

	describe('always sets the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 2,
		xSecondAs(val, coll)[[2]] %is% val
	) +

	run()

message('xSecondAs (-)')

	over(val, coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) < 2,
		xSecondAs(val, coll)
	) +

	run()
