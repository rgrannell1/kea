
kea ::: load_test_dependencies()

to_test       <- function (coll1, coll2) {}
body(to_test) <- kea ::: Must_Be_Equal_Length_To(coll1, coll2)

message("Must_Be_Equal_Length_To")

	over(coll) +

	describe('a collection always has the same length as itself') +
	worksWhen(
		suchThat $ is_collection(coll),
		to_test(coll, coll)
	) +

	run()

	over(coll1, coll2) +

	describe('fails when the lenghts arent equal') +
	failsWhen(
		is_collection(coll1) && is_collection(coll2) &&
		length(coll1) != length(coll2),
		to_test(coll1, coll2)
	) +

	run()
