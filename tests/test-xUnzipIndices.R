
kiwi ::: load_test_dependencies(environment())


message("xUnzipIndices (+)")

	over(coll) +

	describe("the empty collection always yields the list") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xUnzipIndices(coll)  %is% list()
	) +

	describe("otherwise made of two-tuples") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		all(sapply( xUnzipIndices(coll), length ) == 2)
	) +

	describe("the first column is made of indices") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		{

			firsts <- lapply(xUnzipIndices(coll), '[[', 1)

			firsts %is% as.list(seq_along(coll))
		}
	) +

	describe("the second column is the values") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		{

			seconds <- lapply(xUnzipIndices(coll), '[[', 2)

			seconds %is% as.list(coll)
		}
	) +

	run()

message("xUnzipIndices (-)")

	over(fn, coll) +

	describe("coll must always be a collection") +
	failsWhen(
		!is_collection(coll),
		xUnzipIndices(coll)
	) +

	run()
