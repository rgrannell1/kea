
kiwi ::: load_test_dependencies(environment())

message('xSliceString')

	over(str, nums) +

	describe('selecting at zero is character(0)') +
	holdsWhen(
		is.character(str) && length(str) == 1 &&
		!is.na(str) &&
		is_collection(nums) && length(nums) == 0,
		xSliceString(str, 0) %is% character(0),
		xSliceString(str, nums) %is% character(0)
	) +

	describe('slicing with indices is identity') +
	holdsWhen(
		is.character(str) && length(str) == 1 &&
		!is.na(str),
		xSliceString( str, seq_len(nchar(str)) ) == str
	) +

	run()
