
kea ::: load_test_dependencies(environment())

message("xGather")

	over(coll) +

	describe("xGather always runs") +
	worksWhen(
		is_collection(coll),

		xGather(coll)
	) +

	describe("xGather returns a list of lists") +
	holdsWhen(
		is_collection(coll),

		all( vapply(xGather(coll), is.list, logical(1)) )
	) +

	run()
