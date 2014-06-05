
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message('xFourthOf')

	over(coll) +

	describe('always returns the correct element') +
	when(
		is_collection(coll) && length(coll) >= 4,
		xFourthOf(coll) %equals% coll[[4]]
	) +
	run()
