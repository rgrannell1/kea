
kea ::: load_test_dependencies()

message("xChoose")

	over(coll) +

	describe("choosing with empty collection is empty collection") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xChoose(0, list()) %is% list(),
		xChoose(1, list()) %is% list(),
		xChoose(2, list()) %is% list()
	) +

	describe("choosing with 1 is as.list") +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xChoose(1, coll) %is% as.list(coll)
	) +

	run()
