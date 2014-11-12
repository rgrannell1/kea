
kea ::: load_test_dependencies()

message('xFromWords')

	over(strs) +

	describe("xFromWords of character(0) is character(0)") +
	holdsWhen(
		suchThat $ is_empty_collection(strs),

		xFromWords(strs) %is% character(0)
	) +

	describe("xFromWords is length-one") +
	holdsWhen(
		suchThat $ not_empty_collection(strs),

		length(xFromWords(strs)) == 1
	) +

	run()
