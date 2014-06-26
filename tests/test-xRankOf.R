
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xRankOf (+)")

	over(nums) +

	describe("rank of empty collection is integer(0)") +
	holdsWhen(
		is_collection(nums) && length(nums) == 0 && !any(is.na(nums)),
		xRankOf(nums) %is% integer(0)
	) +

	describe("the rank of one number is one.") +
	holdsWhen(
		is.numeric(nums) && length(nums) == 1 && !any(is.na(nums)),
		xRankOf(nums) == 1
	) +

	describe("sorting the rank of numbers is seq_along nums") +
	holdsWhen(
		is.numeric(nums) && !any(is.na(nums)),
		sort(xRankOf(nums)) %is% seq_along(nums)
	) +

	run()
