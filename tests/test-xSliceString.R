
kiwi ::: load_test_dependencies(environment())

message('xSliceString')

	over(str, nums) +

	describe('selecting at zero is character(0)') +
	holdsWhen(
		is.character(str) && length(str) == 1 &&
		!is.na(str) &&
		is_collection(nums) && length(nums) == 0,

		xSliceString(0, str) %is% character(0),
		xSliceString(nums, str) %is% character(0)
	) +

	describe('slicing with indices is identity') +
	holdsWhen(
		is.character(str) && length(str) == 1 && !is.na(str),

		xSliceString( seq_len(nchar(str)), str) == str
	) +

	run()
