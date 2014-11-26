
kea ::: load_test_dependencies(environment())

message("xZip")

	over(coll) +

	it('xZip of the empty collection is list()') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xZip(coll) %is% list()
	) +

	it('xZip of a collection is list(coll)') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xZip(coll) %is% list(as.list(coll))
	) +

	it('xZip of two collection makes two-tuples') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xZip_(coll, coll) %is% unname(lapply(coll, function (elem) list(elem, elem)))
	) +

	run()
