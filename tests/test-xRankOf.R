
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xRankOf (+)")

	over(nums) +

	describe("rank of empty collection is integer(0)") +
	when(
		is_collection(nums) && length(nums) == 0,
		xRankOf(nums) %is% integer(0)
	) +

	describe("the rank of one number is one.") +
	when(
		is.numeric(nums) && length(nums) == 1,
		xRankOf(nums) == 1
	) +

	describe("sorting the rank of numbers is seq_along nums") +
	when(
		is.numeric(nums),
		sort(xRankOf(nums)) %is% seq_along(nums)
	) +

	run()
