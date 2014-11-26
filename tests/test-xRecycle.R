
kea ::: load_test_dependencies(environment())

message("xRecycle")

	over(coll, num) +

	it("recycling the empty collection is the empty collection") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xRecycle(num, coll) %is% list()
	) +

	it("recycling the empty collection is the empty collection (named)") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xRecycle(num, coll) %is% as_named(list())
	) +

	it("recycling to current length is identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xRecycle(length(coll), coll) %is% as.list(coll)
	) +

	it("recycling returns correct length") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		suchThat $ is_collection(coll),

		length(xRecycle(length(coll), coll)) == length(coll)
	) +

	run()
