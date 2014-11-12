
kea ::: load_test_dependencies(environment())

message("xFlatten")

	over(num, coll) +

	describe("flattening empty collection is empty list") +
	holdsWhen(
		is_numeric(num) && length(num) == 1 &&
		!is.na(num) && round(unlist(num)) == num && num > 0 &&
		suchThat $ is_empty_collection(coll),

		xFlatten(num, coll) %is% list()
	) +

	describe("flattening to one is unlist") +
	holdsWhen(
		suchThat $ is_collection(coll),

		xFlatten(1, coll) %is% as.list( unlist(unname(coll)) ),
		xFlatten(1, xFlatten(1, coll)) %is% xFlatten(1, coll)
	) +

	describe("flattening atomic is as.list") +
	holdsWhen(
		is_numeric(num) && length(num) == 1 &&
		!is.na(num) && round(unlist(num)) == num && num > 0 &&
		is_atomic(coll),

		xFlatten(num, coll) %is% unname(as.list(coll))
	) +

	describe("flattening to infinity is as list") +
	holdsWhen(
		is_numeric(num) && length(num) == 1 &&
		!is.na(num) && round(unlist(num)) == num && num > 0 &&
		is_atomic(coll),

		xFlatten(Inf, coll) %is% unname(as.list(coll))
	) +

	run()
