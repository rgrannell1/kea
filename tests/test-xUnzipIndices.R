
kea ::: load_test_dependencies(environment())

message("xUnzipIndices")

	over(coll) +

	describe("the empty collection always yields the list") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),
		xUnzipIndices(coll)  %is% list()
	) +

	describe("the empty collection always yields the list (named)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),
		xUnzipIndices(coll)  %is% as_named(list())
	) +

	describe("otherwise made of two-tuples") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		all(sapply( xUnzipIndices(coll), length ) == 2)
	) +

	describe("first elem is index") +
	holdsWhen(
		is_collection(coll),

		{
			unzipped <- xUnzipIndices(coll)

			all( vapply(seq_along(coll), function (ith) {
				unzipped[[ith]][[1]] ==  ith
			}, integer(1)) )
		}
	) +

	describe("second elem is value") +
	holdsWhen(
		is_collection(coll),

		{
			unzipped <- xUnzipIndices(coll)

			all( vapply(seq_along(coll), function (ith) {
				identical( unzipped[[ith]][[2]], coll[[ith]] )
			}, integer(1)) )
		}
	) +

	describe("names are preserved") +
	holdsWhen(
		is_collection(coll),

		names(xUnzipIndices(coll)) %is% names(coll)
	) +

	run()
