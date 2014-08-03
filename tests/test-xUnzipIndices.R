
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

			seconds %is% as.list(unname(coll))
		}
	) +

	run()
