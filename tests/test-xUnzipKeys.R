
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xUnzipKeys (+)")

	over(coll) +

	describe("the empty collection always yields the list") +
	when(
		is_collection(coll) && length(coll) == 0,
		xUnzipKeys(coll)  %is% list()
	) +

	run()

message("xUnzipKeys (-)")

	over(fn, coll) +

	describe("coll must always be a collection") +
	failswhen(
		!is_collection(coll),
		xUnzipKeys(coll)
	) +

	run()
