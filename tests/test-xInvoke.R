
kea ::: load_test_dependencies(environment())

unit_test('xInvoke')

	over(coll) +

	# unname since apply uses names as argument names.
	it("invoking preserves list") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xInvoke(as.list, coll) %is% as.list(coll)
	) +

	run()
