
kea ::: load_test_dependencies(environment())

message("xZipKeys")

	over(colls) +

	describe('xZipKeys of the empty collection is list()') +
	holdsWhen(
		is_collection(colls) && length(colls) == 0,
		xZipKeys(colls) %is% list()
	) +

	describe('xZipKeys zips names for pairs') +
	holdsWhen(
		is_collection(colls) && is.character(colls) && length(colls) > 0,
		{
			pairs <- unname(lapply(colls, function (elem) list(elem, elem)))

			xZipKeys(pairs) %is% as.list(structure(unname(colls), names = unname(colls)))
		}
	) +

	run()
