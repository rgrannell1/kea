
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xZipKeys (+)")

	over(coll) +

	describe('xZipKeys of the empty collection is list()') +
	when(
		is_collection(coll) && length(coll) == 0,
		xZipKeys(coll) %is% list()
	) +

	describe('xZipKeys zips names for pairs') +
	when(
		is_collection(coll) && is.character(coll) && length(coll) > 0,
		{
			pairs <- lapply(coll, function (elem) list(elem, elem))

			xZipKeys(pairs) %is% as.list(structure(coll, names = coll))
		}
	) +

	run()
