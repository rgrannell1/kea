
kiwi ::: load_test_dependencies(environment())

message("xScan")

    over(val, coll) +

	describe("scan with empty coll is list of val") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,

		xScan(list, val, coll) %is% list(val)
	) +

	run()
