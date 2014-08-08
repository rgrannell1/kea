
kea ::: load_test_dependencies(environment())

message('xFromWords')

	over(strs) +

	describe("xFromWords of character(0) is character(0)") +
	holdsWhen(
		is_collection(strs) && length(strs) == 0,

		xFromWords(strs) %is% character(0)
	) +

	describe("xFromWords is length-one") +
	holdsWhen(
		is_character(strs) && !anyNA(strs) && length(strs) > 0,

		length(xFromWords(strs)) == 1
	) +

	run()
