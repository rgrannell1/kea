
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xSecondOf')

	over(coll) +

	describe('always returns the correct element') +
	when(
		is_collection(coll) && length(coll) >= 2,
		xSecondOf(coll) %equals% coll[[2]]
	) +
	run()
