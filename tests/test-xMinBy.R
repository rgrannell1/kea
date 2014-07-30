
kiwi ::: load_test_dependencies(environment())

message("xMinBy")

	over(coll) +

	describe("min of no elements fails") +
	failsWhen(
		is_collection(coll) && length(coll) == 0,

		xMinBy(identity, coll)
	) +

	describe("minby of one element is that element") +
	holdsWhen(
		is_collection(coll) && length(coll) == 1,

		xMinBy(identity, coll) %is% coll[[1]]
	) +

	describe("minby of numbers is the smallest number") +
	holdsWhen(
		is_numeric(coll) && length(coll) > 0,

		xMinBy(identity, coll) %is% min(coll)
	) +

	run()

	over(num, coll) +

	describe("minby a constant function is coll_1") +
	holdsWhen(
		is_collection(coll) && length(coll) > 0 &&
		is_numeric(num) && length(num) == 1 &&
		!is.na(num),
		xMinBy(xCapture(num), coll) %is% coll[[1]]
	) +

	run()
