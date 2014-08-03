
kea ::: load_test_dependencies(environment())

message("xZip")

	over(coll) +

	describe('xZip of the empty collection is list()') +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xZip(coll) %is% list()
	) +

	describe('xZip of a collection is list(coll)') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xZip(coll) %is% list(as.list(coll))
	) +

	describe('xZip of two collection makes two-tuples') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		xZip_(coll, coll) %is% unname(lapply(coll, function (elem) list(elem, elem)))
	) +

	run()
