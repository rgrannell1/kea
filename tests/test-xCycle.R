
kiwi ::: load_test_dependencies(environment())

message('xCycle')

	over(coll) +

	describe("multiples of coll length are identity") +
	holdsWhen(
		is_collection(coll),
		xCycle(-length(coll), coll) %is% as.list(coll),
		xCycle(0,             coll) %is% as.list(coll),
		xCycle(+length(coll), coll) %is% as.list(coll)
	) +

	run()
