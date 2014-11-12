
kea ::: load_test_dependencies(environment())

message("xMinBy")

	over(coll) +

	describe("min of no elements fails") +
	failsWhen(
		suchThat $ is_empty_collection(coll)

		xMinBy(identity, coll)
	) +

	describe("minby of one element is that element") +
	holdsWhen(
		suchThat $ is_collection(coll) && length(coll) == 1,

		xMinBy(identity, coll) %is% coll[[1]]
	) +

	over(num, coll) +

	describe("minby a constant function is coll_1") +
	holdsWhen(
		suchThat $ not_empty_collection(coll) &&
		is.numeric(num) && length(num) == 1 &&
		!is.na(num),

		xMinBy(xCapture(num), coll) %is% coll[[1]]
	) +

	run()
