
kea ::: load_test_dependencies()

message("xOrderOf")

	over(coll) +

	describe("order of empty collection is integer(0)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xOrderOf(coll) %is% integer(0)
	) +

	describe("order of empty collection is integer(0)") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xOrderOf(coll) %is% as_named(integer(0))
	) +

	run()

	over(nums) +

	describe("order of nums is seq along nums") +
	holdsWhen(
		is_numeric(nums) && !is.na(unlist(nums)) && !is.nan(unlist(nums)),

		xOrderOf(as.list( sort(unlist(nums)) )) %is% seq_along(nums)
	) +

	run()
