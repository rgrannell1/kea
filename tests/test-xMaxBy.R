
kea ::: load_test_dependencies(environment())

message("xMaxBy")

	over(coll) +

	describe("max of no elements fails") +
	failsWhen(
		suchThat $ is_empty_collection(coll)

		xMaxBy(identity, coll)
	) +

	describe("maxby of one element is that element") +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) == 1,

		xMaxBy(identity, coll) %is% coll[[1]]
	) +

	over(num, coll) +

	describe("maxby a constant function is coll_1") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) &&
		is.numeric(num) && length(num) == 1 &&
		!is.na(num),
		xMaxBy(xCapture(num), coll) %is% coll[[1]]
	) +

	run()
