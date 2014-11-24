
kea ::: load_test_dependencies(environment())

message('xFromLines')

	over(strs) +

	describe("xFromLines of character(0) is character(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(strs),

		xFromLines(strs) %is% character(0)
	) +

	describe("xFromLines is length-one") +
	holdsWhen(
		is_character(strs) && !anyNA(strs) && length(strs) > 0,

		length(xFromLines(strs)) == 1
	) +

	run()
