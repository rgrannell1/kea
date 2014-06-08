
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xThirdOf')

	over(coll) +

	describe('always returns the correct element') +
	when(
		is_collection(coll) && length(coll) >= 3,
		xThirdOf(coll) %equals% coll[[3]]
	) +
	run()

message('xThirdOf (-)')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) < 3,
		xThirdOf(coll)
	) +

	run()
