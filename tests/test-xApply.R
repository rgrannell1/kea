
kea ::: load_test_dependencies(environment())

unit_test('xApply')

	over(coll) +

	# unname since apply uses names as argument names.
	it("apply to list yields list") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xApply(list, coll) %is% as.list(coll)
	) +

	run()
