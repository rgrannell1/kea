
kea ::: load_test_dependencies(environment())

message("xZipKeys")

	over(colls) +

	describe('xZipKeys of the empty collection is list()') +
	holdsWhen(
		suchThat $ is_empty_collection(colls),

		xZipKeys(colls) %is% list()
	) +

	describe('xZipKeys zips names for pairs') +
	holdsWhen(
		and_(suchThat $ is_empty_collection(colls), is.character)(colls),

		{
			pairs <- unname(lapply(colls, function (elem) list(elem, elem)))

			xZipKeys(pairs) %is% as.list(structure(unname(colls), names = unname(colls)))
		}
	) +

	run()
