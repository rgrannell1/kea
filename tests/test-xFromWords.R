
kea ::: load_test_dependencies(environment())

message('xFromWords')

	over(strs) +

	describe("xFromWords of character(0) is character(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(strs),

		xFromWords(strs) %is% character(0)
	) +

	describe("xFromWords is length-one") +
	holdsWhen(
		and_(suchThat $ not_empty_character, suchThat $ without_na)(strs),

		length(xFromWords(strs)) == 1
	) +

	run()
