
kea ::: load_test_dependencies(environment())

message("xSlice")

	over(coll, nums) +

	describe("slicing with no indices is the empty list") +
	holdsWhen(
		suchThat $ is_empty_collection(nums) &&
		suchThat $ not_named_collection(coll),

		xSlice(nums, coll) %is% list()
	) +

	describe("slicing with no indices is the empty list (named)") +
	holdsWhen(
		suchThat $ is_empty_collection(nums) &&
		suchThat $ is_named_collection(coll),

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
