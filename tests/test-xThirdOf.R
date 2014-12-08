
kea ::: load_test_dependencies(environment())

unit_test('xThirdOf')

	over(coll) +

	it('always returns the third element') +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) >= 3,

		xThirdOf(coll) %is% coll[[3]]
	) +
	run()

message('xThirdOf')

	over(coll) +

	it('fails when the collection is too short') +
	failsWhen(
		suchThat $ is_collection(coll) && length(coll) < 3,

		xThirdOf(coll)
	) +

	run()
