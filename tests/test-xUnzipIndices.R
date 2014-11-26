
kea ::: load_test_dependencies(environment())

unit_test("xUnzipIndices")

	over(coll) +

	it("the empty collection always yields the list") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xUnzipIndices(coll)  %is% list()
	) +

	it("the empty collection always yields the list (named)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xUnzipIndices(coll)  %is% as_named(list())
	) +

	it("otherwise made of two-tuples") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		all(sapply( xUnzipIndices(coll), length ) == 2)
	) +

	it("first elem is index") +
	holdsWhen(
		suchThat $ is_collection(coll),

		{
			unzipped <- xUnzipIndices(coll)

			all( vapply(seq_along(coll), function (ith) {
				unzipped[[ith]][[1]] ==  ith
			}, integer(1)) )
		}
	) +

	it("second elem is value") +
	holdsWhen(
		suchThat $ is_collection(coll),

		{
			unzipped <- xUnzipIndices(coll)

			all( vapply(seq_along(coll), function (ith) {
				identical( unzipped[[ith]][[2]], coll[[ith]] )
			}, integer(1)) )
		}
	) +

	it("names are preserved") +
	holdsWhen(
		suchThat $ is_collection(coll),

		names(xUnzipIndices(coll)) %is% names(coll)
	) +

	run()
