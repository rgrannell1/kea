
kea ::: load_test_dependencies(environment())

unit_test("xRank")

	over(nums) +

	it("rank of empty collection is integer(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(nums) && !any(is.na(nums)) && !is_named(nums),

		xRank(nums) %is% integer(0)
	) +


	it("rank of empty collection is integer(0) (named)") +
	holdsWhen(
		suchThat $ is_empty_collection(nums) && !any(is.na(nums)) && is_named(nums),

		xRank(nums) %is% as_named(integer(0))
	) +

	it("the rank of one number is one.") +
	holdsWhen(
		is_numeric(nums) && length(nums) == 1 && !any(is.na(nums)),

		unname(xRank(nums) == 1)
	) +

	it("sorting the rank of numbers is seq_along nums") +
	holdsWhen(
		is_numeric(nums) && !any(is.na(nums)) && length(nums) > 0,

		unname( sort(xRank(nums)) ) %is% seq_along(nums)
	) +

	it("ranking keeps names, though unordered") +
	holdsWhen(
		is_numeric(nums) && !any(is.na(nums)) && length(nums) > 0 && is_named(nums),

		sort( names(xRank(nums)) ) %is% sort(names(nums))
	) +

	run()
