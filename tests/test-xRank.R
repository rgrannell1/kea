
kea ::: load_test_dependencies(environment())

message("xRank")

	over(nums) +

	describe("rank of empty collection is integer(0)") +
	holdsWhen(
		is_collection(nums) && length(nums) == 0 && !any(is.na(nums)) && !is_named(nums),
		xRank(nums) %is% integer(0)
	) +


	describe("rank of empty collection is integer(0) (named)") +
	holdsWhen(
		is_collection(nums) && length(nums) == 0 && !any(is.na(nums)) && is_named(nums),
		xRank(nums) %is% as_named(integer(0))
	) +

	describe("the rank of one number is one.") +
	holdsWhen(
		is_numeric(nums) && length(nums) == 1 && !any(is.na(nums)),

		unname(xRank(nums) == 1)
	) +

	describe("sorting the rank of numbers is seq_along nums") +
	holdsWhen(
		is_numeric(nums) && !any(is.na(nums)) && length(nums) > 0,

		unname( sort(xRank(nums)) ) %is% seq_along(nums)
	) +

	describe("ranking keeps names, though unordered") +
	holdsWhen(
		is_numeric(nums) && !any(is.na(nums)) && length(nums) > 0 && is_named(nums),

		names(xRank(nums)) %is% sort(names(nums))
	) +

	run()
2