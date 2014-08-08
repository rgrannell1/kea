
kea ::: load_test_dependencies(environment())

message('xApply')

	over(coll) +

	# unname since apply uses names as argument names.
	describe("apply to list yields list") +
	holdsWhen(
		is_collection(coll),
		xApply(list, unname(coll)) %is% as.list(unname(coll))
	) +

	run()
