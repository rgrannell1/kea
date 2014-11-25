
kea ::: load_test_dependencies(environment())

message("xMedianBy")

	over(nums) +

	describe('the median of empty vector is double(0)') +
	holdsWhen(
		suchThat $ is_empty_collection(nums),

		xMedianBy(identity, nums) %is% double(0)
	) +

	describe('the median of one number is itself') +
	holdsWhen(
		is_numeric(nums) && length(nums) == 1 &&
		!any(is.na(unlist(nums)) || is.nan(unlist(nums))),

		xMedianBy(identity, nums) == unname(nums)
	) +

	run()

	over(nums) +

	describe('fails for na or nan values') +
	failsWhen(
		is_numeric(nums) && any(is.na(unlist(nums)) || is.nan(unlist(nums))),

		xMedianBy(identity, nums)
	) +

	run()
