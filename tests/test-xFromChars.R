
kea ::: load_test_dependencies(environment())

message('xFromChars')

	over(strs) +

	describe("xFromChars of character(0) is character(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(strs),

		xFromChars(strs) %is% character(0)
	) +

	describe("xFromChars is length-one") +
	holdsWhen(
		is_character(strs) && !anyNA(strs) && length(strs) > 0,

		length(xFromChars(strs)) == 1
	) +

	run()
