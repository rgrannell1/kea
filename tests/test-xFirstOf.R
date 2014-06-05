
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message('xFirstOf')

	over(coll) +

	describe('always returns the correct element') +
	when(
		is_collection(coll) && length(coll) >= 1,
		xFirstOf(coll) %equals% coll[[1]]
	) +
	run()
