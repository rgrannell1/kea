
kea ::: load_test_dependencies()

message("xRecycle")

	over(coll, num) +

	describe("recycling the empty collection is the empty collection") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xRecycle(num, coll) %is% list()
	) +

	describe("recycling the empty collection is the empty collection (named)") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xRecycle(num, coll) %is% as_named(list())
	) +

	describe("recycling to current length is identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xRecycle(length(coll), coll) %is% as.list(coll)
	) +

	describe("recycling returns correct length") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		suchThat $ is_collection(coll),

		length(xRecycle(length(coll), coll)) == length(coll)
	) +

	run()
