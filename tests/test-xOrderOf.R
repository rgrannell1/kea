
kiwi ::: load_test_dependencies(environment())

message("xOrderOf")

	over(coll) +

	describe("order of empty collection is integer(0)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),

		xOrderOf(coll) %is% integer(0)
	) +

	describe("order of empty collection is integer(0)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),

		xOrderOf(coll) %is% as_named(integer(0))
	) +

	run()

	over(nums) +

	describe("order of nums is seq along nums") +
	holdsWhen(
		is_numeric(nums) && !is.na(nums) && !is.nan(nums),

		xOrderOf(sort(nums)) %is% seq_along(nums)
	) +

	run()
