
kiwi ::: load_test_dependencies(environment())


message('xLastAs (+)')

	over(val, coll) +

	describe('always sets the correct element') +
	holdsWhen(
		is_collection(coll) && length(coll) >= 1,
		xLastAs(val, coll)[[length(coll)]] %is% val
	) +

	run()

message('xLastAs (-)')

	over(val, coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) == 0,
		xLastAs(val, coll)
	) +

	run()
