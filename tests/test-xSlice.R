
kea ::: load_test_dependencies(environment())

message("xSlice")

	over(coll, nums) +

	describe("slicing with no indices is the empty list") +
	holdsWhen(
		is_collection(nums) && length(nums) == 0 &&
		suchThat $ is_collection(coll) && !is_named(coll),

		xSlice(nums, coll) %is% list()
	) +

	describe("slicing with no indices is the empty list (named)") +
	holdsWhen(
		is_collection(nums) && length(nums) == 0 &&
		suchThat $ is_collection(coll) && is_named(coll),

		xSlice(nums, coll) %is% as_named(list())
	) +

	describe("slicing indices is identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSlice(seq_along(coll), coll) %is% as.list(coll)
	) +

	describe("slicing preserve slice names") +
	holdsWhen(
		suchThat $ is_collection(coll),

		names(xSlice(seq_along(coll), coll)) %is% names(coll)
	) +

	run()
