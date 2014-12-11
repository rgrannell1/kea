
kea ::: load_test_dependencies(environment())

unit_test("xScan")

    over(val, coll) +

	it("scan with empty coll is list of val") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xScan(list, val, coll) %is% list(val)
	) +

	it("returns a collection as long as both inputs") +
	holdsWhen(
		suchThat $ is_collection(coll),

		length(xScan(function (...) val, val, coll)) == length(coll) + 1
	) +

	run()
