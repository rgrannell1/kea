
kea ::: load_test_dependencies(environment())

message("xUnzipKeys")

	over(coll) +

	describe("the empty collection always yields the list") +
	holdsWhen(
		suchThat $ is_empty_collection(coll)

		xUnzipKeys(coll)  %is% list()
	) +

	run()

message("xUnzipKeys")

	over(fn, coll) +

	describe("coll must always be a collection") +
	failsWhen(
		suchThat $ not_collection(coll),

		xUnzipKeys(coll)
	) +

	run()
