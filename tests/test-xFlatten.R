
kiwi ::: load_test_dependencies(environment())

message("xFlatten")

	over(num, coll) +

	describe("flattening empty collection is empty list") +
	holdsWhen(
		is.numeric(num) && length(num) == 1 &&
		!is.na(num) && round(num) == num && num > 0 &&
		is_collection(coll) && length(coll) == 0,

		xFlatten(num, coll) %is% list()
	) +

	describe("flattening to one is unlist") +
	holdsWhen(
		is_collection(coll),

		xFlatten(1, coll) %is% as.list(unlist(unname(coll)))
	) +

	describe("flattening atomic is as.list") +
	holdsWhen(
		is.numeric(num) && length(num) == 1 &&
		!is.na(num) && round(num) == num && num > 0 &&
		is_atomic(coll),

		xFlatten(num, coll) %is% unname(as.list(coll))
	) +

	describe("flattening to infinity is as list") +
	holdsWhen(
		is.numeric(num) && length(num) == 1 &&
		!is.na(num) && round(num) == num && num > 0 &&
		is_atomic(coll),

		xFlatten(Inf, coll) %is% unname(as.list(coll))
	) +

	run()
