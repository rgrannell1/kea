
kea ::: load_test_dependencies(environment())

message('xCycle')

	over(num, coll) +

	it("multiples of coll length are identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xCycle(-length(coll), coll) %is% as.list(coll),
		xCycle(0,             coll) %is% as.list(coll),
		xCycle(+length(coll), coll) %is% as.list(coll)
	) +

	it("names are preserved under inverse cycling") +
	holdsWhen(
		is.numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num)) &&
		!is.infinite(num) &&
		suchThat $ is_collection(coll),

		xCycle(-num, names(xCycle(num, coll)) ) %is% as.list(names(coll))

	) +

	run()
