
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xLastAs (+)')

	over(val, coll) +

	describe('always sets the correct element') +
	when(
		is_collection(coll) && length(coll) >= 1,
		xLastAs(val, coll)[[length(coll)]] %is% val
	) +

	run()

message('xLastAs (-)')

	over(val, coll) +

	describe('fails when the collection is too short') +
	failswhen(
		is_collection(coll) && length(coll) == 0,
		xLastAs(val, coll)
	) +

	run()
