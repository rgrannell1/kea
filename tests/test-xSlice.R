
kea ::: load_test_dependencies(environment())

unit_test("xSlice")

	over(coll, nums) +

	it("slicing with no indices is the empty list") +
	holdsWhen(
		suchThat $ is_empty_collection(nums) &&
		suchThat $ not_named_collection(coll),

		xSlice(nums, coll) %is% list()
	) +

	it("slicing with no indices is the empty list (named)") +
	holdsWhen(
		suchThat $ is_empty_collection(nums) &&
		suchThat $ is_named_collection(coll),

		xSlice(nums, coll) %is% as_named(list())
	) +

	it("slicing indices is identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xSlice(seq_along(coll), coll) %is% as.list(coll)
	) +

	it("slicing preserve slice names") +
	holdsWhen(
		suchThat $ is_collection(coll),

		names(xSlice(seq_along(coll), coll)) %is% names(coll)
	) +

	run()
