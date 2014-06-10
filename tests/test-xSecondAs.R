
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xSecondAs (+)')

	over(val, coll) +

	describe('always sets the correct element') +
	when(
		is_collection(coll) && length(coll) >= 2,
		xSecondAs(val, coll)[[2]] %equals% val
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
