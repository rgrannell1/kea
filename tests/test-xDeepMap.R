
kea ::: load_test_dependencies(environment())

unit_test("xDeepMap")

	over(coll) +

	it('deepmapping over empty list is empty list') +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xDeepMap(identity, coll) %is% keep_names(list(), coll)
	) +

	it('deep-map any coll with identity is that coll') +
	holdsWhen(
		suchThat $ is_collection(coll),

		xDeepMap(identity, coll) %is% as.list(coll)
	) +

	run()
