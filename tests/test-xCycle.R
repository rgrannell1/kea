
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xCycle (+)')

	over(coll) +

	describe("multiples of coll length are identity") +
	when(
		is_collection(coll),
		xCycle(-length(coll), coll) %equals% as.list(coll),
		xCycle(0,             coll) %equals% as.list(coll),
		xCycle(+length(coll), coll) %equals% as.list(coll)
	) +

	run()
