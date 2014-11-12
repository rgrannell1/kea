
kea ::: load_test_dependencies()

message("xGather")

	over(coll) +

	describe("xGather always runs") +
	worksWhen(
		suchThat $ is_collection(coll),

		xGather(coll)
	) +

	describe("xGather returns a list of lists") +
	holdsWhen(
		suchThat $ is_collection(coll),

		all( vapply(xGather(coll), is.list, logical(1)) )
	) +

	run()
