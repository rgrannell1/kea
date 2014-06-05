
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message("xZip (+)")

	over(coll) +

	describe('xZip of the empty collection is list()') +
	when(
		is_collection(coll) && length(coll) == 0,
		xZip(coll) %equals% list()
	) +

	describe('xZip of a collection is list(coll)') +
	when(
		is_collection(coll) && length(coll) > 0,
		xZip(coll) %equals% list(as.list(coll))
	) +

	run()
