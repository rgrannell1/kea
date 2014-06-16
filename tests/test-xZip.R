
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xZip (+)")

	over(coll) +

	describe('xZip of the empty collection is list()') +
	when(
		is_collection(coll) && length(coll) == 0,
		xZip(coll) %is% list()
	) +

	describe('xZip of a collection is list(coll)') +
	when(
		is_collection(coll) && length(coll) > 0,
		xZip(coll) %is% list(as.list(coll))
	) +

	describe('xZip of two collection makes two-tuples') +
	when(
		is_collection(coll) && length(coll) > 0,
		xZip_(coll, coll) %is% lapply(coll, function (elem) list(elem, elem))
	) +

	run()
