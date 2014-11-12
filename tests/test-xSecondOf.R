
kea ::: load_test_dependencies()

message('xSecondOf')

	over(coll) +

	describe('always returns the correct element') +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) >= 2,

		xSecondOf(coll) %is% coll[[2]]
	) +
	run()

message('xSecondOf')

	over(coll) +

	describe('fails when the collection is too short') +
	failsWhen(
		suchThat $ is_collection(coll) && length(coll) < 2,

		xSecondOf(coll)
	) +

	run()
