
kea ::: load_test_dependencies(environment())

message("xZip")

	over(coll) +

	describe('xZip of the empty collection is list()') +
	holdsWhen(
		suchThat $ is_empty_collection(coll)

		xZip(coll) %is% list()
	) +

	describe('xZip of a collection is list(coll)') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xZip(coll) %is% list(as.list(coll))
	) +

	describe('xZip of two collection makes two-tuples') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xZip_(coll, coll) %is% unname(lapply(coll, function (elem) list(elem, elem)))
	) +

	run()
