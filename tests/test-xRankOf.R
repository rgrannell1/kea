
kiwi ::: load_test_dependencies(environment())


message("xRankOf (+)")

	over(nums) +

	describe("rank of empty collection is integer(0)") +
	holdsWhen(
		is_collection(nums) && length(nums) == 0 && !any(is.na(nums)) && !is_named(nums),
		xRankOf(nums) %is% integer(0)
	) +


	describe("rank of empty collection is integer(0) (named)") +
	holdsWhen(
		is_collection(nums) && length(nums) == 0 && !any(is.na(nums)) && is_named(nums),
		xRankOf(nums) %is% as_named(integer(0))
	) +

	describe("the rank of one number is one.") +
	holdsWhen(
		is.numeric(nums) && length(nums) == 1 && !any(is.na(nums)),
		xRankOf(nums) == 1
	) +

	describe("sorting the rank of numbers is seq_along nums") +
	holdsWhen(
		is.numeric(nums) && !any(is.na(nums)) && length(nums) > 0,
		sort(xRankOf(nums)) %is% seq_along(nums)
	) +

	run()
