
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xUnzipIndices (+)")

	over(coll) +

	describe("the empty collection always yields the list") +
	when(
		is_collection(coll) && length(coll) == 0,
		xUnzipIndices(coll)  %equals% list()
	) +

	describe("otherwise made of two-tuples") +
	when(
		is_collection(coll) && length(coll) > 0,
		all(sapply( xUnzipIndices(coll), length ) == 2)
	) +

	describe("the first column is made of indices") +
	when(
		is_collection(coll) && length(coll) > 0,
		{

			firsts <- lapply(xUnzipIndices(coll), '[[', 1)

			firsts %equals% as.list(seq_along(coll))
		}
	) +

	describe("the second column is the values") +
	when(
		is_collection(coll) && length(coll) > 0,
		{

			seconds <- lapply(xUnzipIndices(coll), '[[', 2)

			seconds %equals% as.list(coll)
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
