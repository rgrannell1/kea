
kea ::: load_test_dependencies(environment())

message("xRecycle")

	over(coll, num) +

	describe("recycling the empty collection is the empty collection") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		is_collection(coll) && length(coll) == 0 && !is_named(coll),

		xRecycle(num, coll) %is% list()
	) +

	describe("recycling the empty collection is the empty collection (named)") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		is_collection(coll) && length(coll) == 0 && is_named(coll),

		xRecycle(num, coll) %is% as_named(list())
	) +

	describe("recycling to current length is identity") +
	holdsWhen(
		is_collection(coll),

		xRecycle(length(coll), coll) %is% as.list(coll)
	) +

	describe("recycling returns correct length") +
	holdsWhen(
		is_numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 0)) &&
		is_collection(coll),

		length(xRecycle(length(coll), coll)) == length(coll)
	) +

	run()
