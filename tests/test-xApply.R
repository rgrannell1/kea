
kea ::: load_test_dependencies()

message('xApply')

	over(coll) +

	# unname since apply uses names as argument names.
	describe("apply to list yields list") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xApply(list, coll) %is% as.list(coll)
	) +

	run()
