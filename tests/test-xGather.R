
kea ::: load_test_dependencies(environment())

unit_test("xGather")

	over(coll) +

	it("xGather always runs") +
	worksWhen(
		suchThat $ is_collection(coll),

		xGather(coll)
	) +

	it("xGather returns a list of lists") +
	holdsWhen(
		suchThat $ is_collection(coll),

		all( vapply(xGather(coll), is.list, logical(1)) )
	) +

	run()
