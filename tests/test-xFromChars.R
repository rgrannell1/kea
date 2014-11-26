
kea ::: load_test_dependencies(environment())

unit_test('xFromChars')

	over(strs) +

	it("xFromChars of character(0) is character(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(strs),

		xFromChars(strs) %is% character(0)
	) +

	it("xFromChars is length-one") +
	holdsWhen(
		is_character(strs) && !anyNA(strs) && length(strs) > 0,

		length(xFromChars(strs)) == 1
	) +

	run()
