
kea ::: load_test_dependencies(environment())

message("xMeanBy")

	over(nums) +

	describe('the mean of empty vector is double(0)') +
	holdsWhen(
		suchThat $ is_empty_collection(nums),

		xMeanBy(identity, nums) %is% double(0)
	) +

	describe('the mean of one number is itself') +
	holdsWhen(
		is_numeric(nums) && length(nums) == 1 &&
		!any(is.na(unlist(nums)) || is.nan(unlist(nums))),

		xMeanBy(identity, nums) == unname(nums)
	) +

	run()

	over(nums) +

	describe('fails for na or nan values') +
	failsWhen(
		is_numeric(nums) && any(is.na(unlist(nums)) || is.nan(unlist(nums))),

		xMeanBy(identity, nums)
	) +

	run()
