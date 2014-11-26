
kea ::: load_test_dependencies(environment())

message('xFromWords')

	over(strs) +

	it("xFromWords of character(0) is character(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(strs),

		xFromWords(strs) %is% character(0)
	) +

	it("xFromWords is length-one") +
	holdsWhen(
		and_(suchThat $ not_empty_character, suchThat $ without_na)(strs),

		length(xFromWords(strs)) == 1
	) +

	run()
