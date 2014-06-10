
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xFourthAs (+)')

	over(val, coll) +

	describe('always sets the correct element') +
	when(
		is_collection(coll) && length(coll) >= 4,
		xFourthAs(val, coll)[[4]] %equals% val
	) +

	run()

message('xFourthAs (-)')

	over(val, coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) < 4,
		xFourthAs(val, coll)
	) +

	run()
