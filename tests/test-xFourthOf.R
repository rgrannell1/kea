
kea ::: load_test_dependencies()

message('xFourthOf')

	over(coll) +

	describe('always returns the correct element') +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) >= 4,

		xFourthOf(coll) %is% coll[[4]]
	) +
	run()

message('xFourthOf')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		suchThat $ is_collection(coll) && length(coll) < 4,

		xFourthOf(coll)
	) +

	run()
