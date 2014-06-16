
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xFirstOf (+)')

	over(coll) +

	describe('always returns the correct element') +
	when(
		is_collection(coll) && length(coll) >= 1,
		xFirstOf(coll) %is% coll[[1]]
	) +

	run()

message('xFirstOf (-)')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		is_collection(coll) && length(coll) == 0,
		xFirstOf(coll)
	) +

	run()
