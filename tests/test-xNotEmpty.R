
kea ::: load_test_dependencies()

message("xNotEmpty")

	over(coll) +

	describe('xNotEmpty correctly reports lengths.') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		!xNotEmpty(coll)
	) +

	describe('xNotEmpty correctly reports lengths.') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xNotEmpty(coll)
	) +


	run()
