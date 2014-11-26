
kea ::: load_test_dependencies(environment())

message('xSecondOf')

	over(coll) +

	it('always returns the correct element') +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) >= 2,

		xSecondOf(coll) %is% coll[[2]]
	) +
	run()

message('xSecondOf')

	over(coll) +

	it('fails when the collection is too short') +
	failsWhen(
		suchThat $ is_collection(coll) && length(coll) < 2,

		xSecondOf(coll)
	) +

	run()
