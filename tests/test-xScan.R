
kea ::: load_test_dependencies(environment())

message("xScan")

    over(val, coll) +

	it("scan with empty coll is list of val") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xScan(list, val, coll) %is% list(val)
	) +

	run()
