
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xOrderOf (+)")

	over(coll) +

	describe("order of empty collection is integer(0)") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xOrderOf(coll) %is% integer(0)
	) +

	run()

	over(nums) +

	describe("order of nums is seq along nums") +
	holdsWhen(
		is.numeric(nums) && !is.nan(nums),
		xOrderOf(sort(nums)) %is% seq_along(nums)
	) +

	run()
