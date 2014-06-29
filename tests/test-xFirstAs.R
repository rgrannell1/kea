
kiwi ::: load_test_dependencies(environment())


message('xFirstAs (+)')

	over(val, coll) +

	describe('always sets the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 1,
		xFirstAs(val, coll)[[1]] %is% val
	) +

	run()

message('xFirstAs (-)')

	over(val, coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) == 0,
		xFirstAs(val, coll)
	) +

	run()
