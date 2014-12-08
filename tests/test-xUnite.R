
kea ::: load_test_dependencies(environment())

unit_test('xUnite')

	over(coll) +

	it('the union of an empty set is an empty set') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xUnite(coll, coll) %is% list()
	) +

	it('the union of two equals sets is the first set') +
	holdsWhen(
		suchThat $ is_collection(coll),

		xUnite(unique(coll), unique(coll)) %is% as.list(unique(coll))
	) +

	run()
