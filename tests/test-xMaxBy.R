
kiwi ::: load_test_dependencies(environment())

message("xMaxBy")

	over(coll) +

	describe("max of no elements fails") +
	failsWhen(
		is_collection(coll) && length(coll) == 0,

		xMaxBy(identity, coll)
	) +

	describe("maxby of one element is that element") +
	holdsWhen(
		is_collection(coll) && length(coll) == 1,

		xMaxBy(identity, coll) %is% coll[[1]]
	) +

	describe("maxby of numbers is the smallest number") +
	holdsWhen(
		is.numeric(coll) && length(coll) > 0,

		xMaxBy(identity, coll) %is% max(coll)
	) +

	run()

	over(num, coll) +

	describe("maxby a constant function is coll_1") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0 &&
		is.numeric(num) && length(num) == 1 &&
		!is.na(num),
		xMaxBy(xCapture(num), coll) %is% coll[[1]]
	) +

	run()
